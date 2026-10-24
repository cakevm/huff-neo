use crate::prelude::{EnvOverrides, Failure, Outcome, RunnerError, SpecContext, TestResult, TestSpec, TestStatus};
use alloy_evm::{Evm, EvmEnv, EvmFactory, eth::EthEvmFactory};
use alloy_primitives::{Address, Bytes, U256, hex};
use anvil::eth::backend::mem::inspector::AnvilInspector;
use foundry_evm::backend::DatabaseError;
use foundry_evm_core::constants::CHEATCODE_ADDRESS;
use huff_neo_codegen::Codegen;
use huff_neo_utils::ast::huff::MacroDefinition;
use huff_neo_utils::prelude::{Contract, EVMVersion, deploy_bootstrap};
use revm::context::result::{ExecutionResult, Output};
use revm::context::{TransactTo, TxEnv};
use revm::context_interface::block::BlobExcessGasAndPrice;
use revm::context_interface::cfg::GasParams;
use revm::context_interface::cfg::gas_params::Eip2780TxInfo;
use revm::interpreter::{CallInputs, CallOutcome, InterpreterTypes};
use revm::primitives::hardfork::SpecId;
use revm::state::{Account, AccountInfo, Bytecode, EvmStorageSlot, TransactionId};
use revm::{Context, Database, DatabaseCommit, ExecuteCommitEvm, Inspector, MainBuilder, MainContext};
use std::collections::HashMap;

/// Container for EVM environment (cfg + block) and transaction.
#[derive(Clone, Debug, Default)]
pub struct Env {
    pub evm_env: EvmEnv,
    pub tx: TxEnv,
}

/// The test runner allows execution of test macros within an in-memory REVM
/// instance.
#[derive(Debug, Default)]
pub struct TestRunner {
    pub env: Env,
    pub inspector: AnvilInspector,
    pub target_address: Option<Address>,
    /// EVM version the test macros are compiled for
    pub evm_version: EVMVersion,
}

impl TestRunner {
    pub fn new(env: Env, inspector: AnvilInspector, target_address: Option<Address>, evm_version: EVMVersion) -> Self {
        Self { env, inspector, target_address, evm_version }
    }

    /// Set the balance of an account.
    pub fn set_balance<DB>(&self, db: &mut DB, address: Address, amount: U256) -> Result<(), RunnerError>
    where
        DB: Database + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        update_account_info(db, address, |info| info.balance = amount)
    }

    /// Set the value of a storage slot of an account, creating the account if it does not exist.
    pub fn set_storage<DB>(&self, db: &mut DB, address: Address, slot: U256, value: U256) -> Result<(), RunnerError>
    where
        DB: Database + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        let basic_account = db.basic(address).map_err(db_error)?;
        let original_value = db.storage(address, slot).map_err(db_error)?;
        let storage = std::iter::once((slot, EvmStorageSlot::new_changed(original_value, value, TransactionId::ZERO)));

        let mut changes = HashMap::default();
        let account = match basic_account {
            Some(account_info) => Account::from(account_info),
            None => Account::from(AccountInfo::default()).with_created_mark(),
        };
        // The database skips untouched accounts on commit, so a new account must be touched as well
        changes.insert(address, account.with_storage(storage).with_touched_mark());
        db.commit(changes);

        Ok(())
    }

    /// Set the nonce of an account, creating the account if it does not exist.
    pub fn set_nonce<DB>(&self, db: &mut DB, address: Address, nonce: u64) -> Result<(), RunnerError>
    where
        DB: Database + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        update_account_info(db, address, |info| info.nonce = nonce)
    }

    /// Set the code of an account, creating the account if it does not exist.
    pub fn set_code<DB>(&self, db: &mut DB, address: Address, code: String) -> Result<(), RunnerError>
    where
        DB: Database + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        let code = Bytecode::new_raw(Bytes::from(hex::decode(code).expect("Invalid code")));
        update_account_info(db, address, |info| {
            // `extcodehash` reads the stored hash, so it must match the new code
            info.code_hash = code.hash_slow();
            info.code = Some(code);
        })
    }

    /// Deploy arbitrary bytecode to our REVM instance and return the contract address.
    pub fn deploy_code<DB>(&mut self, db: &mut DB, code: String) -> Result<Address, RunnerError>
    where
        DB: Database + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        // Wrap code in a bootstrap constructor
        let (bootstrap_code, _) = deploy_bootstrap(code.len() / 2, 0);
        let bootstrap = format!("{bootstrap_code}{code}");

        let mut env = self.env.clone();
        env.tx.chain_id = Some(env.evm_env.chainid());
        env.tx.kind = TransactTo::Create;
        // The following should never panic, as any potential compilation error
        // as well as an uneven number of hex nibbles should be caught in the
        // compilation process.
        env.tx.data = hex::decode(bootstrap).expect("Invalid bootstrap bytecode").into();

        self.set_balance(db, self.env.tx.caller, U256::MAX)?;
        // TODO: Make spec_id configurable, allow to log during create with the inspector?
        let mut evm = Context::mainnet().with_block(env.evm_env.block_env).with_cfg(env.evm_env.cfg_env).with_db(db).build_mainnet();

        // Send our CREATE transaction
        let er = evm.transact_commit(env.tx).map_err(|e| RunnerError::TransactError(format!("{e:?}")))?;

        // Check if deployment was successful
        let address = match er {
            ExecutionResult::Success { output: Output::Create(_, Some(addr)), .. } => addr,

            ExecutionResult::Revert { gas, output, .. } => {
                let gas_used = gas.tx_gas_used();
                return Err(RunnerError::DeploymentError(format!("Deployment reverted gas_used={gas_used}, output={output:?}")));
            }
            ExecutionResult::Halt { reason, gas, .. } => {
                let gas_used = gas.tx_gas_used();
                return Err(RunnerError::DeploymentError(format!("Deployment halted gas_used={gas_used}, reason={reason:?}")));
            }
            ExecutionResult::Success { output, .. } => {
                return Err(RunnerError::DeploymentError(format!("Deployment failed with unexpected output: {output:?}")));
            }
        };

        Ok(address)
    }

    /// Perform a call to a deployed contract
    pub fn call<DB>(
        &mut self,
        name: String,
        db: &mut DB,
        transact_to: Address,
        value: U256,
        data: String,
    ) -> Result<TestResult, RunnerError>
    where
        DB: Database<Error = DatabaseError> + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        self.set_balance(db, self.env.tx.caller, U256::MAX)?;
        self.call_in_env(name, db, self.env.clone(), transact_to, value, data)
    }

    /// Perform a call to a deployed contract in the given environment instead of the runner's; the
    /// sender must already hold the funds for the call
    fn call_in_env<DB>(
        &mut self,
        name: String,
        db: &mut DB,
        mut env: Env,
        transact_to: Address,
        value: U256,
        data: String,
    ) -> Result<TestResult, RunnerError>
    where
        DB: Database<Error = DatabaseError> + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        env.tx.chain_id = Some(env.evm_env.chainid());
        env.tx.kind = TransactTo::Call(transact_to);
        env.tx.data = hex::decode(data).expect("Invalid calldata").into();
        env.tx.value = value;

        // The sender may be the deployer of the test or an address set by a `caller` decorator
        let sender = db.basic(env.tx.caller).map_err(db_error)?;
        env.tx.nonce = sender.map_or(0, |account| account.nonce);

        let mut inspector = self.inspector.clone();
        let mut cheatcode_guard = CheatcodeGuard::default();
        let mut evm = EthEvmFactory::default().create_evm_with_inspector(db, env.evm_env.clone(), (&mut cheatcode_guard, &mut inspector));

        // Send our CALL transaction
        let execution_result = evm.transact_commit(env.tx).map_err(|e| RunnerError::TransactError(format!("{e:?}")))?;
        // Release the inspectors so the cheatcode guard can be read
        drop(evm);

        // Extract execution params
        let gas_used = execution_result.tx_gas_used();
        let outcome = match &execution_result {
            ExecutionResult::Success { output, .. } => Outcome::Return { data: hex::encode(output.data()) },
            ExecutionResult::Revert { output, .. } => Outcome::Revert { data: hex::encode(output) },
            ExecutionResult::Halt { reason, .. } => Outcome::Halt { reason: format!("{reason:?}") },
        };
        let mut failures = Vec::new();
        if cheatcode_guard.called {
            failures.push(Failure {
                expectation: String::from("cheatcodes"),
                message: format!("the test calls Foundry cheatcodes at {CHEATCODE_ADDRESS}, which hnc test does not support yet"),
            });
        }
        let passed = matches!(outcome, Outcome::Return { .. }) && failures.is_empty();

        // Check if the transaction was successful
        let (return_data, revert_reason) = match &execution_result {
            ExecutionResult::Success { output, .. } => {
                if let Output::Call(b) = output {
                    if b.is_empty() { (None, None) } else { (Some(hex::encode(b)), None) }
                } else {
                    (None, Some("Unexpected transaction kind (CREATE)".to_string()))
                }
            }
            ExecutionResult::Revert { output, .. } => {
                if output.is_empty() {
                    (None, None)
                } else {
                    (Some(hex::encode(output)), None)
                }
            }
            ExecutionResult::Halt { reason, .. } => (None, Some(format!("Transaction halted with reason: {reason:?}"))),
        };

        // Return our test result
        // NOTE: We subtract the intrinsic base cost of the CALL transaction, which depends on the
        // active hardfork (EIP-2780 reprices it from Amsterdam).
        Ok(TestResult {
            name,
            return_data,
            gas: gas_used.saturating_sub(call_base_gas(env.evm_env.cfg_env.spec, value)),
            status: TestStatus::from_passed(passed),
            revert_reason,
            passed,
            outcome,
            failures,
            inspector,
            source_map: None,
            address: None,
            source_code: None,
            bytecode: None,
            source_files: Vec::new(),
        })
    }

    /// Compile a test macro and run it in an in-memory REVM instance.
    pub fn run_test<DB>(&mut self, db: &mut DB, m: &MacroDefinition, contract: &Contract) -> Result<TestResult, RunnerError>
    where
        DB: Database<Error = DatabaseError> + DatabaseCommit + std::fmt::Debug,
        <DB as Database>::Error: std::fmt::Debug,
    {
        let evm_version = self.evm_version;

        let name = m.name.to_owned();

        // Update the table size in the contract
        let contract = Codegen::update_table_size(&evm_version, contract)?;

        // Compile the passed test macro
        let mut scope_mgr = huff_neo_utils::scope::ScopeManager::new();
        scope_mgr.push_macro(m, 0);
        let res = Codegen::macro_to_bytecode(&evm_version, m, &contract, &mut scope_mgr, 0, false, false)?;

        // Generate table bytecode for compiled test macro. Test macros run standalone (no
        // bootstrap, no upstream dedup target), so base_offset = 0 and the external map is empty.
        let table_out = Codegen::gen_table_bytecode(&contract, res, 0, &std::collections::HashMap::new())?;
        let bytecode = format!("{}{}", table_out.body, table_out.tables);
        let source_map = table_out.source_map;

        // Deploy compiled test macro
        let address = match self.target_address {
            Some(address) => {
                self.set_code(db, address, bytecode.clone())?;
                address
            }
            None => self.deploy_code(db, bytecode.clone())?,
        };

        // Apply the setup requested by the test decorator. The sender is funded first, so a `deal`
        // to the sender sets its balance instead.
        let spec = TestSpec::from_decorator(m.decorator.as_ref(), &contract, SpecContext { this: address, caller: self.env.tx.caller })?;
        let mut env = self.env.clone();
        apply_env_overrides(&mut env, &spec.env);
        self.set_balance(db, env.tx.caller, U256::MAX)?;
        for slot in &spec.storage {
            self.set_storage(db, slot.address, slot.slot, slot.value)?;
        }
        for balance in &spec.balances {
            self.set_balance(db, balance.address, balance.value)?;
        }
        for code in &spec.code {
            self.set_code(db, code.address, hex::encode(&code.value))?;
        }
        for nonce in &spec.nonces {
            self.set_nonce(db, nonce.address, nonce.value)?;
        }

        // Call the deployed test
        let mut res = self.call_in_env(name, db, env, address, spec.value, hex::encode(&spec.calldata))?;

        // Check how the test ended against what its decorators expect. A test that did not return
        // kept none of its state, and the outcome failure already says why.
        res.failures.extend(spec.expectations.check(&res.outcome, res.gas, &contract));
        if matches!(res.outcome, Outcome::Return { .. }) {
            res.failures.extend(spec.expectations.check_post_state(db)?);
        }
        res.passed = res.failures.is_empty();
        res.status = TestStatus::from_passed(res.passed);

        // Add source map and address for debugging
        res.source_map = Some(source_map);
        res.address = Some(address);

        // Get the full source code for debugging
        // Use the flattened source from the contract if available
        res.source_code = contract.flattened_source.clone();
        res.bytecode = Some(bytecode.clone());
        res.source_files = contract.source_files.clone();

        Ok(res)
    }
}

/// Notes whether the test calls Foundry's cheatcode address, which this runner does not serve yet;
/// such a call would otherwise hit an empty account and silently succeed
#[derive(Debug, Default)]
struct CheatcodeGuard {
    called: bool,
}

impl<CTX, INTR: InterpreterTypes> Inspector<CTX, INTR> for CheatcodeGuard {
    fn call(&mut self, _context: &mut CTX, inputs: &mut CallInputs) -> Option<CallOutcome> {
        self.called |= inputs.target_address == CHEATCODE_ADDRESS;
        None
    }
}

/// Changes the info of an account, such as its balance or code, creating the account if it does not exist
fn update_account_info<DB>(db: &mut DB, address: Address, update: impl FnOnce(&mut AccountInfo)) -> Result<(), RunnerError>
where
    DB: Database + DatabaseCommit,
    <DB as Database>::Error: std::fmt::Debug,
{
    let account = match db.basic(address).map_err(db_error)? {
        Some(original_info) => {
            let mut info = original_info.clone();
            update(&mut info);
            Account::from(original_info).with_info(info)
        }
        None => {
            // The default code hash is the empty code's, which `extcodehash` reads for an account without code
            let mut info = AccountInfo::default();
            update(&mut info);
            Account::from(info).with_created_mark()
        }
    };

    let mut changes = HashMap::default();
    // The database skips untouched accounts on commit, so a new account must be touched as well
    changes.insert(address, account.with_touched_mark());
    db.commit(changes);

    Ok(())
}

/// Wraps a database failure as a runner error
pub(crate) fn db_error(err: impl std::fmt::Debug) -> RunnerError {
    RunnerError::GenericError(format!("{err:?}"))
}

/// Sets the transaction and block fields a test overrides
fn apply_env_overrides(env: &mut Env, overrides: &EnvOverrides) {
    let EnvOverrides {
        caller,
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
    let block = &mut env.evm_env.block_env;

    if let Some(caller) = caller {
        env.tx.caller = caller;
    }
    if let Some(gas_limit) = gas_limit {
        env.tx.gas_limit = gas_limit;
        // A transaction may not use more gas than its block allows
        block.gas_limit = block.gas_limit.max(gas_limit);
    }
    if let Some(gas_price) = gas_price {
        env.tx.gas_price = gas_price;
    }
    if let Some(number) = block_number {
        block.number = number;
    }
    if let Some(timestamp) = timestamp {
        block.timestamp = timestamp;
    }
    if let Some(basefee) = basefee {
        block.basefee = basefee;
        // The test transaction pays no fee, so a base fee above its gas price must not reject it
        env.evm_env.cfg_env.disable_base_fee = true;
    }
    if let Some(blob_gasprice) = blob_basefee {
        block.blob_excess_gas_and_price = Some(BlobExcessGasAndPrice { excess_blob_gas: 0, blob_gasprice });
    }
    if let Some(coinbase) = coinbase {
        block.beneficiary = coinbase;
    }
    if let Some(chain_id) = chain_id {
        env.evm_env.cfg_env.chain_id = chain_id;
    }
    if let Some(prevrandao) = prevrandao {
        block.prevrandao = Some(prevrandao);
    }
    if let Some(slot_number) = slot_number {
        block.slot_num = slot_number;
    }
}

/// Intrinsic gas of a call transaction to another account, excluding calldata and access lists
///
/// This is 21000 before Amsterdam. From Amsterdam (EIP-2780) it is the sender base cost plus a cold
/// access of the recipient, and an extra charge when value is transferred.
fn call_base_gas(spec: SpecId, value: U256) -> u64 {
    let eip2780 = spec.is_enabled_in(SpecId::AMSTERDAM).then_some(Eip2780TxInfo { value, is_self_transfer: false });
    GasParams::new_spec(spec).initial_tx_gas(&[], false, 0, 0, 0, eip2780).initial_regular_gas
}
