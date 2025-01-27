use crate::{errors::RunnerError, runner::TestRunner, types::TestResult};
use alloy_primitives::{Address, U256};
use huff_neo_utils::prelude::{Contract, MacroDefinition};
use std::{borrow::Borrow, rc::Rc};

/// The runner module
pub mod runner;

/// The report module
pub mod report;

/// The cheats module
pub mod cheats;

/// The types module
pub mod types;

/// The errors module
pub mod errors;

/// Re-export the Inspector from anvil crate
pub use anvil::eth::backend::mem::inspector::Inspector;
use foundry_debugger::Debugger;
use foundry_evm::backend::Backend;
use foundry_evm::fork::CreateFork;
use foundry_evm::traces::{InternalTraceMode, SparsedTraceArena, TraceKind};
use revm::db::CacheDB;
use revm::primitives::{Env, SpecId};

/// Prelude wraps all modules within the crate
pub mod prelude {
    pub use crate::{errors::*, report::*, runner::*, types::*};
}

/// A vector of shared references to test macro definitions
pub type TestMacros<'t> = Vec<&'t MacroDefinition>;

/// Builder for the HuffTester
#[derive(Clone, Debug, Default)]
pub struct HuffTesterConfig {
    /// The address which will be used to deploy the initial contracts and send all
    /// transactions
    pub sender: Option<Address>,
    /// The initial balance for each one of the deployed smart contracts
    pub initial_balance: U256,
    /// The EVM spec to use
    pub evm_spec: Option<SpecId>,
    /// The fork to use at launch
    pub fork: Option<CreateFork>,
    /// Whether or not to collect coverage info
    pub coverage: bool,
    /// Whether or not to collect debug info
    pub debug: bool,
    /// Whether to enable steps tracking in the tracer.
    pub decode_internal: InternalTraceMode,
    /// Whether to enable call isolation
    pub isolation: bool,
    /// Whether to enable Odyssey features.
    pub odyssey: bool,
}

impl HuffTesterConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn sender(mut self, sender: Address) -> Self {
        self.sender = Some(sender);
        self
    }

    pub fn initial_balance(mut self, initial_balance: U256) -> Self {
        self.initial_balance = initial_balance;
        self
    }

    pub fn evm_spec(mut self, spec: SpecId) -> Self {
        self.evm_spec = Some(spec);
        self
    }

    pub fn with_fork(mut self, fork: Option<CreateFork>) -> Self {
        self.fork = fork;
        self
    }

    pub fn set_coverage(mut self, enable: bool) -> Self {
        self.coverage = enable;
        self
    }

    pub fn set_debug(mut self, enable: bool) -> Self {
        self.debug = enable;
        self
    }

    pub fn set_decode_internal(mut self, mode: InternalTraceMode) -> Self {
        self.decode_internal = mode;
        self
    }

    pub fn enable_isolation(mut self, enable: bool) -> Self {
        self.isolation = enable;
        self
    }

    pub fn odyssey(mut self, enable: bool) -> Self {
        self.odyssey = enable;
        self
    }
}

/// The core struct of the huff-tests crate.
///
/// A `HuffTester` struct is instantiated with an AST of a contract that contains test
/// macros. The struct can be consumed by the [HuffTester::execute](execute) method,
/// returning a vector of [TestResult](TestResult) structs.
pub struct HuffTester<'t> {
    /// The AST of the contract
    pub ast: &'t Contract,

    /// The test macros
    pub macros: TestMacros<'t>,

    /// The test runner
    pub runner: TestRunner,

    /// The configuration for the test runner
    pub config: HuffTesterConfig,
}

/// HuffTester implementation
impl<'t> HuffTester<'t> {
    /// Create a new instance of `HuffTester` from a contract's AST.
    pub fn new(ast: &'t Contract, match_: Rc<Option<String>>, inspectors: Inspector, config: HuffTesterConfig, env: Env) -> Self {
        Self {
            ast,
            macros: {
                // Filter all macros within the AST for `test` macros only
                let mut macros: TestMacros<'t> = ast.macros.iter().filter(|m| m.test).collect();
                // If the match flag is present, only retain the test macro
                // that was queried
                if let Some(match_) = match_.borrow() {
                    macros.retain(|m| m.name == *match_);
                }
                macros
            },
            runner: TestRunner::new(env, inspectors),
            config,
        }
    }

    /// Execute tests
    pub fn execute(mut self) -> Result<Vec<TestResult>, RunnerError> {
        // Check if any test macros exist
        if self.macros.is_empty() {
            return Err(RunnerError::GenericError(String::from("No test macros found.")));
        }

        // Execute our tests and return a vector of the results
        let results = self
            .macros
            .into_iter()
            .map(|macro_def| {
                let db = Backend::spawn(self.config.fork.take());
                let mut cache_db = CacheDB::new(db);
                self.runner.run_test(&mut cache_db, macro_def, self.ast)
            })
            .collect::<Result<Vec<TestResult>, RunnerError>>();

        if !self.config.debug {
            return results;
        }

        //
        // Run the debugger
        //
        let Ok(ref results_vec) = results else {
            return results;
        };
        if results_vec.len() != 1 {
            return Err(RunnerError::GenericError("Debugging is only supported for a single test".to_string()));
        }
        let Some(test_result) = results_vec.first() else {
            return Err(RunnerError::GenericError("No test result found".to_string()));
        };

        let Some(trace_area) = test_result.clone().inspector.tracer else {
            return Err(RunnerError::GenericError("No trace found".to_string()));
        };

        let traces = [(TraceKind::Execution, SparsedTraceArena { arena: trace_area.into_traces(), ignored: Default::default() })];

        let builder = Debugger::builder().traces(traces.iter().filter(|(t, _)| t.is_execution()).cloned().collect());

        let mut debugger = builder.build();
        debugger.try_run_tui().map_err(|e| RunnerError::GenericError(format!("{:?}", e)))?;

        results
    }
}
