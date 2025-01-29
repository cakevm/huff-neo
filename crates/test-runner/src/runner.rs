use crate::prelude::{RunnerError, TestResult, TestStatus};
use alloy_primitives::{hex, Address, B256, U256};
use anvil::eth::backend::mem::inspector::Inspector;
use huff_neo_codegen::Codegen;
use huff_neo_utils::ast::huff::{DecoratorFlag, MacroDefinition};
use huff_neo_utils::prelude::{pad_n_bytes, CompilerError, Contract, EVMVersion};
use revm::primitives::{Account, AccountInfo, AccountStatus};
use revm::{
    inspector_handle_register,
    primitives::{Env, ExecutionResult, Output, TransactTo, CANCUN},
    Database, DatabaseCommit, Evm,
};
use std::collections::HashMap;

/// The test runner allows execution of test macros within an in-memory REVM
/// instance.
#[derive(Debug, Default)]
pub struct TestRunner {
    pub env: Env,
    pub inspector: Inspector,
}

impl TestRunner {
    pub fn new(env: Env, inspectors: Inspector) -> Self {
        Self { env, inspector: inspectors }
    }

    /// Set the balance of an account.
    pub fn set_balance<DB>(&self, db: &mut DB, address: Address, amount: U256) -> Result<(), RunnerError>
    where
        DB: Database + DatabaseCommit,
        <DB as Database>::Error: std::fmt::Debug,
    {
        let basic_account = db.basic(address).map_err(|e| RunnerError::GenericError(format!("{:?}", e)))?;

        let mut changes = HashMap::default();
        let account = match basic_account {
            Some(mut account_info) => {
                account_info.balance = amount;
                Account { info: account_info, storage: Default::default(), status: AccountStatus::Touched }
            }
            None => Account {
                info: AccountInfo { balance: amount, nonce: 0, code_hash: B256::ZERO, code: None },
                storage: Default::default(),
                status: AccountStatus::Created,
            },
        };

        changes.insert(address, account);
        db.commit(changes);

        Ok(())
    }

    /// Deploy arbitrary bytecode to our REVM instance and return the contract address.
    pub fn deploy_code<DB>(&mut self, db: &mut DB, code: String) -> Result<Address, RunnerError>
    where
        DB: Database + DatabaseCommit,
        <DB as Database>::Error: std::fmt::Debug,
    {
        // Wrap code in a bootstrap constructor
        let contract_length = code.len() / 2;
        let constructor_length = 0;
        let mut bootstrap_code_size = 9;
        let contract_size = if contract_length < 256 {
            format!("60{}", pad_n_bytes(format!("{contract_length:x}").as_str(), 1))
        } else {
            bootstrap_code_size += 1;

            format!("61{}", pad_n_bytes(format!("{contract_length:x}").as_str(), 2))
        };
        let contract_code_offset = if (bootstrap_code_size + constructor_length) < 256 {
            format!("60{}", pad_n_bytes(format!("{:x}", bootstrap_code_size + constructor_length).as_str(), 1))
        } else {
            bootstrap_code_size += 1;

            format!("61{}", pad_n_bytes(format!("{:x}", bootstrap_code_size + constructor_length).as_str(), 2))
        };
        let bootstrap = format!("{contract_size}80{contract_code_offset}3d393df3{code}");

        let mut env = self.env.clone();
        env.tx.transact_to = TransactTo::Create;
        // The following should never panic, as any potential compilation error
        // as well as an uneven number of hex nibbles should be caught in the
        // compilation process.
        env.tx.data = hex::decode(bootstrap).expect("Invalid bootstrap bytecode").into();

        self.set_balance(db, self.env.tx.caller, U256::MAX)?;
        // TODO: Make spec_id configurable, allow to log during create with the inspector?
        let mut evm = Evm::builder().with_spec_id(CANCUN).with_env(Box::new(env)).with_db(db).build();

        // Send our CREATE transaction
        let er = evm.transact_commit().map_err(|e| RunnerError::TransactError(format!("{:?}", e)))?;

        // Check if deployment was successful
        let address = match er {
            ExecutionResult::Success { output: Output::Create(_, Some(addr)), .. } => addr,

            ExecutionResult::Revert { gas_used, output } => {
                return Err(RunnerError::DeploymentError(format!("Deployment reverted gas_used={}, output={:?}", gas_used, output)));
            }
            ExecutionResult::Halt { reason, gas_used } => {
                return Err(RunnerError::DeploymentError(format!("Deployment halted gas_used={}, reason={:?}", gas_used, reason)));
            }
            ExecutionResult::Success { output, .. } => {
                return Err(RunnerError::DeploymentError(format!("Deployment failed with unexpected output: {:?}", output)));
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
        DB: Database + DatabaseCommit,
        <DB as Database>::Error: std::fmt::Debug,
    {
        let mut env = self.env.clone();
        env.tx.transact_to = TransactTo::Call(transact_to);
        env.tx.data = hex::decode(data).expect("Invalid calldata").into();
        env.tx.value = value;

        self.set_balance(db, env.tx.caller, U256::MAX)?;
        let mut evm = Evm::builder()
            .with_spec_id(CANCUN)
            .with_env(Box::new(env))
            .with_db(db)
            .with_external_context(self.inspector.clone())
            .append_handler_register(inspector_handle_register)
            .build();

        // Send our CALL transaction
        let execution_result = evm.transact_commit().map_err(|e| RunnerError::TransactError(format!("{:?}", e)))?;

        // Extract execution params
        let (gas_used, status) = match execution_result {
            ExecutionResult::Success { gas_used, .. } => (gas_used, TestStatus::Success),
            ExecutionResult::Revert { gas_used, .. } => (gas_used, TestStatus::Revert),
            ExecutionResult::Halt { gas_used, .. } => (gas_used, TestStatus::Revert),
        };

        // Check if the transaction was successful
        let (return_data, revert_reason) = match execution_result {
            ExecutionResult::Success { output, .. } => {
                if let Output::Call(b) = output {
                    if b.is_empty() {
                        (None, None)
                    } else {
                        (Some(hex::encode(b)), None)
                    }
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
            ExecutionResult::Halt { reason, .. } => (None, Some(format!("Transaction halted with reason: {:?}", reason))),
        };

        // Return our test result
        // NOTE: We subtract 21000 gas from the gas result to account for the
        // base cost of the CALL.
        Ok(TestResult { name, return_data, gas: gas_used - 21000, status, revert_reason, inspector: evm.context.external })
    }

    /// Compile a test macro and run it in an in-memory REVM instance.
    pub fn run_test<DB>(&mut self, db: &mut DB, m: &MacroDefinition, contract: &Contract) -> Result<TestResult, RunnerError>
    where
        DB: Database + DatabaseCommit,
        <DB as Database>::Error: std::fmt::Debug,
    {
        // TODO: set to non default
        let evm_version = EVMVersion::default();

        let name = m.name.to_owned();

        // Compile the passed test macro
        let res = match Codegen::macro_to_bytecode(&evm_version, m, contract, &mut vec![m], 0, &mut Vec::default(), false, None) {
            Ok(res) => res,
            Err(e) => return Err(RunnerError::CompilerError(CompilerError::CodegenError(e))),
        };

        // Generate table bytecode for compiled test macro
        let bytecode = match Codegen::gen_table_bytecode(&evm_version, contract, res) {
            Ok(bytecode) => bytecode,
            Err(e) => return Err(RunnerError::CompilerError(CompilerError::CodegenError(e))),
        };

        // Deploy compiled test macro
        let address = self.deploy_code(db, bytecode)?;

        // Set environment flags passed through the test decorator
        let mut data = String::default();
        let mut value = U256::ZERO;
        if let Some(decorator) = &m.decorator {
            for flag in &decorator.flags {
                match flag {
                    DecoratorFlag::Calldata(s) => {
                        // Strip calldata of 0x prefix, if it is present.
                        data = if let Some(s) = s.strip_prefix("0x") { s.to_owned() } else { s.to_owned() };
                    }
                    DecoratorFlag::Value(v) => value = U256::from_be_bytes(*v),
                }
            }
        }

        // Call the deployed test
        let res = self.call(name, db, address, value, data)?;
        Ok(res)
    }
}
