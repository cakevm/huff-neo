use crate::prelude::{EnvOverrides, Failure, HuffFunction, Outcome, RunnerError, SpecContext, TestResult, TestSpec, TestStatus};
use crate::spec::skipped;
use alloy_evm::EvmEnv;
use alloy_primitives::{Address, Bytes, U256, hex};
use foundry_evm::backend::{Backend, DatabaseExt};
use foundry_evm::constants::CALLER;
use foundry_evm::core::decode::SkipReason;
use foundry_evm::core::evm::EthEvmNetwork;
use foundry_evm::executors::{Executor, ExecutorBuilder, RawCallResult};
use foundry_evm::fork::CreateFork;
use foundry_evm::inspectors::CheatsConfig;
use foundry_evm::traces::TraceRequirements;
use foundry_evm_networks::NetworkConfigs;
use huff_neo_codegen::Codegen;
use huff_neo_utils::ast::huff::MacroDefinition;
use huff_neo_utils::bytecode::LabelIndices;
use huff_neo_utils::prelude::{Contract, EVMVersion, deploy_bootstrap};
use huff_neo_utils::scope::ScopeManager;
use revm::context::TxEnv;
use revm::context::result::HaltReason;
use revm::context_interface::block::BlobExcessGasAndPrice;
use revm::context_interface::cfg::GasParams;
use revm::context_interface::cfg::gas_params::Eip2780TxInfo;
use revm::interpreter::SuccessOrHalt;
use revm::primitives::hardfork::SpecId;
use revm::state::Bytecode;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

/// Foundry's default `--initial-balance`: 2^96 - 1 wei
const DEFAULT_INITIAL_BALANCE: U256 = U256::from_limbs([u64::MAX, u32::MAX as u64, 0, 0]);

/// Container for EVM environment (cfg + block) and transaction.
#[derive(Clone, Debug, Default)]
pub struct Env {
    pub evm_env: EvmEnv,
    pub tx: TxEnv,
}

/// Runs test macros on Foundry's executor, with its cheatcodes, tracer and log collector.
#[derive(Debug)]
pub struct TestRunner {
    /// Environment every test starts from, including the sender of its call
    pub env: Env,
    /// Address the test macros run at instead of being deployed
    pub target_address: Option<Address>,
    /// EVM version the test macros are compiled for
    pub evm_version: EVMVersion,
    /// Settings of the Foundry cheatcodes the tests can call
    pub cheats_config: Arc<CheatsConfig>,
    /// What the tracer records of each test
    pub trace_requirements: TraceRequirements,
    /// Whether to collect the logs of each test, for console logs in the report
    pub collect_logs: bool,
    /// Whether to print console logs while a test runs
    pub live_logs: bool,
    /// Whether to run every call of a test as its own transaction, as Foundry's `--isolate` does
    pub isolation: bool,
    /// The balance a deployed test and its sender start with
    pub initial_balance: U256,
    /// The network families the executor runs for
    pub networks: NetworkConfigs,
    /// The chain every test forks, or none to start each test on an empty chain
    pub fork: Option<CreateFork>,
}

impl Default for TestRunner {
    fn default() -> Self {
        // Send from Foundry's default sender, as `hnc test` does
        let mut env = Env::default();
        env.tx.caller = CALLER;
        Self::new(env, None, EVMVersion::default(), Arc::new(cheats_config(CheatsConfig::default())))
    }
}

/// Settings for Foundry cheatcodes in `hnc test`. A failed `vm.assert*` reverts, as in `forge test`,
/// so the test stops there and the failure survives a `vm.revertToState`.
pub fn cheats_config(config: CheatsConfig) -> CheatsConfig {
    CheatsConfig { assertions_revert: true, ..config }
}

impl TestRunner {
    pub fn new(env: Env, target_address: Option<Address>, evm_version: EVMVersion, cheats_config: Arc<CheatsConfig>) -> Self {
        Self {
            env,
            target_address,
            evm_version,
            cheats_config,
            trace_requirements: TraceRequirements::none(),
            collect_logs: false,
            live_logs: false,
            isolation: false,
            initial_balance: DEFAULT_INITIAL_BALANCE,
            networks: NetworkConfigs::default(),
            fork: None,
        }
    }

    /// A fresh executor for one test, on an empty chain or the runner's fork
    pub fn new_executor(&self) -> Result<Executor<EthEvmNetwork>, RunnerError> {
        let backend = Backend::spawn(self.fork.clone()).map_err(|e| RunnerError::GenericError(format!("Failed to spawn backend: {e}")))?;
        let executor = ExecutorBuilder::<EthEvmNetwork>::new()
            .inspectors(|stack| {
                let stack = stack
                    .cheatcodes(self.cheats_config.clone())
                    .trace_requirements(self.trace_requirements)
                    .enable_isolation(self.isolation);
                if self.collect_logs || self.live_logs { stack.logs(self.live_logs) } else { stack }
            })
            .spec_id(self.env.evm_env.cfg_env.spec)
            .gas_limit(self.env.tx.gas_limit)
            .build(self.env.evm_env.clone(), self.env.tx.clone(), backend, self.networks);
        Ok(executor)
    }

    /// Set the balance of an account.
    pub fn set_balance(&self, executor: &mut Executor<EthEvmNetwork>, address: Address, amount: U256) -> Result<(), RunnerError> {
        executor.set_balance(address, amount).map_err(db_error)
    }

    /// Deploy arbitrary bytecode and return the contract address.
    pub fn deploy_code(&self, executor: &mut Executor<EthEvmNetwork>, code: &str) -> Result<Address, RunnerError> {
        // Wrap code in a bootstrap constructor
        let (bootstrap_code, _) = deploy_bootstrap(code.len() / 2, 0);
        let bootstrap = decode_hex(&format!("{bootstrap_code}{code}"), "bytecode")?;

        let sender = self.env.tx.caller;
        self.set_balance(executor, sender, self.initial_balance)?;
        use_sender_nonce(executor, sender)?;
        let deployed = executor
            .deploy(sender, bootstrap, U256::ZERO, None)
            .map_err(|e| RunnerError::DeploymentError(format!("Deployment failed: {e}")))?;
        Ok(deployed.address)
    }

    /// Perform a call to a deployed contract
    pub fn call(
        &self,
        name: String,
        executor: &mut Executor<EthEvmNetwork>,
        transact_to: Address,
        value: U256,
        data: String,
    ) -> Result<TestResult, RunnerError> {
        self.set_balance(executor, self.env.tx.caller, self.initial_balance)?;
        let calldata = decode_hex(&data, "calldata")?;
        self.call_from(name, executor, self.env.tx.caller, transact_to, value, calldata)
    }

    /// Perform a call to a deployed contract from the given sender, in the executor's current
    /// environment; the sender must already hold the funds for the call
    fn call_from(
        &self,
        name: String,
        executor: &mut Executor<EthEvmNetwork>,
        sender: Address,
        transact_to: Address,
        value: U256,
        calldata: Bytes,
    ) -> Result<TestResult, RunnerError> {
        // The sender may be the deployer of the test or an address set by a `caller` decorator
        use_sender_nonce(executor, sender)?;
        let result =
            executor.transact_raw(sender, transact_to, calldata, value).map_err(|e| RunnerError::TransactError(format!("{e:?}")))?;

        // `vm.skip(true)` ends the test with a payload Foundry records, so it is told apart from a revert
        let outcome = match result.skip_reason() {
            Some(SkipReason(reason)) => Outcome::Skip { reason },
            None => outcome_of(&result),
        };
        // Foundry's failure flag, which a DSTest-style `fail()` sets by writing the `failed` slot of the
        // cheatcode address, fails the test as in forge
        let mut failures = Vec::new();
        if executor.has_global_failure(&result.state_changeset) {
            failures.push(Failure { expectation: String::from("failed"), message: String::from("the test set Foundry's failure flag") });
        }
        let passed = matches!(outcome, Outcome::Return { .. }) && failures.is_empty();
        let (return_data, revert_reason) = match &outcome {
            Outcome::Return { data } | Outcome::Revert { data } => ((!data.is_empty()).then(|| data.clone()), None),
            Outcome::Halt { reason } => (None, Some(format!("Transaction halted with reason: {reason}"))),
            Outcome::Skip { .. } => (None, None),
        };

        // NOTE: We subtract the intrinsic base cost of the CALL transaction, which depends on the
        // active hardfork (EIP-2780 reprices it from Amsterdam).
        Ok(TestResult {
            name,
            return_data,
            gas: result.gas_used.saturating_sub(call_base_gas(executor.spec_id(), value)),
            status: TestStatus::from_outcome(&outcome, passed),
            revert_reason,
            passed,
            outcome,
            failures,
            traces: result.traces,
            logs: self.collect_logs.then_some(result.logs),
            functions: Vec::new(),
            source_map: None,
            address: None,
            source_code: None,
            bytecode: None,
            source_files: Vec::new(),
        })
    }

    /// Compile a test macro and run it on the executor, which holds the chain state of this test
    pub fn run_test(
        &self,
        executor: &mut Executor<EthEvmNetwork>,
        m: &MacroDefinition,
        contract: &Contract,
    ) -> Result<TestResult, RunnerError> {
        if let Some(reason) = skipped(m.decorator.as_ref())? {
            return Ok(skipped_result(m.name.clone(), reason));
        }
        let evm_version = &self.evm_version;

        // Update the table size in the contract
        let contract = Codegen::update_table_size(evm_version, contract)?;

        // Compile the passed test macro
        let mut scope_mgr = ScopeManager::new();
        scope_mgr.push_macro(m, 0);
        let res = Codegen::macro_to_bytecode(evm_version, m, &contract, &mut scope_mgr, 0, false)?;
        Codegen::reject_args_start(&res)?;

        let functions = huff_functions(&contract, &res.label_indices);

        // Generate table bytecode for compiled test macro. Test macros run standalone (no
        // bootstrap, no upstream dedup target), so base_offset = 0 and the external map is empty.
        let table_out = Codegen::gen_table_bytecode(&contract, res, 0, &HashMap::new())?;
        let bytecode = format!("{}{}", table_out.body, table_out.tables);
        let source_map = table_out.source_map;

        // Deploy compiled test macro
        let address = match self.target_address {
            Some(address) => {
                let code = Bytecode::new_raw(decode_hex(&bytecode, "bytecode")?);
                executor.set_code(address, code).map_err(db_error)?;
                address
            }
            None => {
                // Like forge, a fresh sender deploys the test with nonce 1, so the test runs at
                // Foundry's test contract address
                let sender = self.env.tx.caller;
                if executor.get_nonce(sender).map_err(db_error)? == 0 {
                    executor.set_account_nonce(sender, 1).map_err(db_error)?;
                }
                let address = self.deploy_code(executor, &bytecode)?;
                // Like forge, the test contract starts with the initial balance
                self.set_balance(executor, address, self.initial_balance)?;
                address
            }
        };
        // On a fork, only allowed accounts may call cheatcodes
        executor.backend_mut().allow_cheatcode_access(address);

        // Apply the setup requested by the test decorator. The sender is funded first, so a `deal`
        // to the sender sets its balance instead. The environment is applied after the deployment,
        // which resets it.
        let spec = TestSpec::from_decorator(m.decorator.as_ref(), &contract, SpecContext { this: address, caller: self.env.tx.caller })?;
        let sender = spec.env.caller.unwrap_or(self.env.tx.caller);
        apply_env_overrides(executor, &self.env, &spec.env);
        self.set_balance(executor, sender, self.initial_balance)?;
        for slot in &spec.storage {
            executor.set_storage_slot(slot.address, slot.slot, slot.value).map_err(db_error)?;
        }
        for balance in &spec.balances {
            self.set_balance(executor, balance.address, balance.value)?;
        }
        for code in &spec.code {
            executor.set_code(code.address, Bytecode::new_raw(Bytes::copy_from_slice(&code.value))).map_err(db_error)?;
        }
        for nonce in &spec.nonces {
            executor.set_account_nonce(nonce.address, nonce.value).map_err(db_error)?;
        }

        // Call the deployed test
        let mut res = self.call_from(m.name.clone(), executor, sender, address, spec.value, Bytes::from(spec.calldata))?;

        // Check how the test ended against what its decorators expect. A test that did not return
        // kept none of its state, and the outcome failure already says why.
        res.failures.extend(spec.expectations.check(&res.outcome, res.gas, &contract));
        if matches!(res.outcome, Outcome::Return { .. }) {
            res.failures.extend(spec.expectations.check_post_state(executor.backend())?);
        }
        res.passed = res.failures.is_empty() && !matches!(res.outcome, Outcome::Skip { .. });
        res.status = TestStatus::from_outcome(&res.outcome, res.passed);

        // Add source map and address for debugging
        res.source_map = Some(source_map);
        res.functions = functions;
        res.address = Some(address);

        // Get the full source code for debugging
        // Use the flattened source from the contract if available
        res.source_code = contract.flattened_source.clone();
        res.bytecode = Some(bytecode);
        res.source_files = contract.source_files.clone();

        Ok(res)
    }
}

/// Where each outlined function of the compiled test starts, so traces can show its calls
fn huff_functions(contract: &Contract, label_indices: &LabelIndices) -> Vec<HuffFunction> {
    contract
        .macros
        .values()
        .filter(|f| f.outlined)
        .filter_map(|f| {
            let entry_pc = label_indices.get_any(&format!("goto_{}", f.name))?;
            // The file name without its directory, like forge shows the contract of an internal call
            let file = f
                .span
                .0
                .iter()
                .find_map(|span| span.file.as_ref())
                .and_then(|source| Path::new(&source.path).file_name().map(|name| name.to_string_lossy().into_owned()));
            Some(HuffFunction { name: f.name.clone(), file, entry_pc, takes: f.takes, returns: f.returns })
        })
        .collect()
}

/// Sends the next transaction of the sender with its current nonce
fn use_sender_nonce(executor: &mut Executor<EthEvmNetwork>, sender: Address) -> Result<(), RunnerError> {
    let nonce = executor.get_nonce(sender).map_err(db_error)?;
    executor.tx_env_mut().nonce = nonce;
    Ok(())
}

/// How a call ended, with Foundry's exit reason turned back into the EVM's halt reason, so
/// `OutOfGas` covers every kind of out-of-gas halt
fn outcome_of(result: &RawCallResult<EthEvmNetwork>) -> Outcome {
    let data = hex::encode(&result.result);
    match result.exit_reason.map(SuccessOrHalt::<HaltReason>::from) {
        Some(SuccessOrHalt::Success(_)) => Outcome::Return { data },
        Some(SuccessOrHalt::Revert) => Outcome::Revert { data },
        Some(SuccessOrHalt::Halt(reason)) => Outcome::Halt { reason: format!("{reason:?}") },
        other => Outcome::Halt { reason: format!("{other:?}") },
    }
}

/// The result of a test a `skip` flag skips, which does not run
fn skipped_result(name: String, reason: Option<String>) -> TestResult {
    TestResult {
        name,
        return_data: None,
        gas: 0,
        status: TestStatus::Skipped,
        revert_reason: None,
        passed: false,
        outcome: Outcome::Skip { reason },
        failures: Vec::new(),
        traces: None,
        logs: None,
        functions: Vec::new(),
        source_map: None,
        address: None,
        source_code: None,
        bytecode: None,
        source_files: Vec::new(),
    }
}

/// Decodes hex the runner was given, such as compiled code or calldata, naming it in the error
fn decode_hex(hex: &str, what: &'static str) -> Result<Bytes, RunnerError> {
    hex::decode(hex).map(Bytes::from).map_err(|e| RunnerError::InvalidHex(what, e.to_string()))
}

/// Wraps a database failure as a runner error
pub(crate) fn db_error(err: impl std::fmt::Debug) -> RunnerError {
    RunnerError::GenericError(format!("{err:?}"))
}

/// Sets the transaction and block fields a test overrides, on top of the runner's environment
///
/// The executor sends every call with a zero base fee and gas price so it can never be rejected for
/// its fees, and its cheatcode inspector shows the test the block and gas price set here.
fn apply_env_overrides(executor: &mut Executor<EthEvmNetwork>, env: &Env, overrides: &EnvOverrides) {
    let EnvOverrides {
        caller: _,
        gas_limit,
        gas_price,
        block_number,
        timestamp,
        basefee,
        blob_basefee,
        coinbase,
        chain_id,
        prevrandao,
        slot_number,
    } = *overrides;
    let mut block = env.evm_env.block_env.clone();

    if let Some(gas_limit) = gas_limit {
        executor.set_gas_limit(gas_limit);
        // A transaction may not use more gas than its block allows
        block.gas_limit = block.gas_limit.max(gas_limit);
    }
    if let Some(number) = block_number {
        block.number = number;
    }
    if let Some(timestamp) = timestamp {
        block.timestamp = timestamp;
    }
    if let Some(basefee) = basefee {
        block.basefee = basefee;
    }
    if let Some(blob_gasprice) = blob_basefee {
        block.blob_excess_gas_and_price = Some(BlobExcessGasAndPrice { excess_blob_gas: 0, blob_gasprice });
    }
    if let Some(coinbase) = coinbase {
        block.beneficiary = coinbase;
    }
    if let Some(chain_id) = chain_id {
        executor.evm_env_mut().cfg_env.chain_id = chain_id;
    }
    if let Some(prevrandao) = prevrandao {
        block.prevrandao = Some(prevrandao);
    }
    if let Some(slot_number) = slot_number {
        block.slot_num = slot_number;
    }

    executor.evm_env_mut().block_env = block.clone();
    executor.inspector_mut().set_block(block);
    executor.inspector_mut().set_gas_price(gas_price.unwrap_or(env.tx.gas_price));
}

/// Intrinsic gas of a call transaction to another account, excluding calldata and access lists
///
/// This is 21000 before Amsterdam. From Amsterdam (EIP-2780) it is the sender base cost plus a cold
/// access of the recipient, and an extra charge when value is transferred.
fn call_base_gas(spec: SpecId, value: U256) -> u64 {
    let eip2780 = spec.is_enabled_in(SpecId::AMSTERDAM).then_some(Eip2780TxInfo { value, is_self_transfer: false });
    GasParams::new_spec(spec).initial_tx_gas(&[], false, 0, 0, 0, eip2780).initial_regular_gas
}
