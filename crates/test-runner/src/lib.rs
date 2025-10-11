use crate::{errors::RunnerError, runner::TestRunner, types::TestResult};
use alloy_primitives::Address;
use huff_neo_utils::prelude::{Contract, MacroDefinition};

/// The runner module
pub mod runner;

/// The report module
pub mod report;

/// The types module
pub mod types;

/// The errors module
pub mod errors;

use alloy_primitives::map::AddressHashMap;
/// Re-export the Inspector from anvil crate
pub use anvil::eth::backend::mem::inspector::AnvilInspector;
use foundry_compilers::{compilers::solc::SolcLanguage, multi::MultiCompilerLanguage};
use foundry_debugger::Debugger;
use foundry_evm::Env;
use foundry_evm::backend::Backend;
use foundry_evm::fork::CreateFork;
use foundry_evm::traces::{InternalTraceMode, SparsedTraceArena, TraceKind};
use foundry_evm_traces::debug::{ArtifactData, ContractSources, SourceData};
use revm::database::CacheDB;
use revm::primitives::hardfork::SpecId;
use std::path::PathBuf;
use std::sync::Arc;

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
    /// The EVM spec to use
    pub evm_spec: Option<SpecId>,
    /// The fork to use at launch
    pub fork: Option<CreateFork>,
    /// Whether to collect debug info
    pub debug: bool,
    /// Whether to enable steps tracking in the tracer.
    pub decode_internal: InternalTraceMode,
    /// Whether to enable call isolation
    pub isolation: bool,
    /// The target address for the contract
    pub target_address: Option<Address>,
}

impl HuffTesterConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn sender(mut self, sender: Address) -> Self {
        self.sender = Some(sender);
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

    pub fn target_address(mut self, target_address: Option<Address>) -> Self {
        self.target_address = target_address;
        self
    }
}

/// The core struct of the huff-tests crate.
///
/// A `HuffTester` struct is instantiated with an AST of a contract that contains test
/// macros. The struct can be consumed by the [HuffTester::execute] method,
/// returning a vector of [TestResult] structs.
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
    pub fn new(ast: &'t Contract, match_test_name: Option<String>, inspector: AnvilInspector, config: HuffTesterConfig, env: Env) -> Self {
        Self {
            ast,
            macros: {
                // Filter all macros within the AST for `test` macros only
                let mut macros: TestMacros<'t> = ast.macros.values().filter(|m| m.test).collect();
                // If the match flag is present, only retain the test macro
                // that was queried
                if let Some(match_test_name) = match_test_name {
                    macros.retain(|m| m.name == *match_test_name);
                }
                macros
            },
            runner: TestRunner::new(env, inspector, config.target_address),
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
                let db = Backend::spawn(self.config.fork.take())
                    .map_err(|_| RunnerError::GenericError("Failed to spawn backend".to_string()))?;
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

        eprintln!("DEBUG: Starting debugger for test: {}", test_result.name);
        eprintln!("DEBUG: Has source map: {}", test_result.source_map.is_some());
        eprintln!("DEBUG: Has source code: {}", test_result.source_code.is_some());
        if let Some(addr) = test_result.address {
            eprintln!("DEBUG: Contract address: {}", addr);
        }

        // Create ContractSources if we have source maps
        let mut debugger_builder = Debugger::builder().traces(traces.iter().filter(|(t, _)| t.is_execution()).cloned().collect());

        // Add identified contracts and source maps for debugging
        let mut identified_contracts = AddressHashMap::default();
        let mut contracts_sources = ContractSources::default();

        if let Some(address) = test_result.address {
            let contract_name = format!("Test::{}", test_result.name);
            identified_contracts.insert(address, contract_name.clone());

            // Add source maps if available
            if let Some(source_map) = &test_result.source_map {
                // Create inner map for file IDs
                let mut file_map = std::collections::HashMap::new();

                // Check if we have multiple source files
                if !test_result.source_files.is_empty() {
                    // Add each source file with its proper file ID
                    for (file_id, (file_path, source_content)) in test_result.source_files.iter().enumerate() {
                        let file_name = PathBuf::from(file_path)
                            .file_stem()
                            .map(|s| s.to_string_lossy().to_string())
                            .unwrap_or_else(|| format!("file_{}", file_id));

                        let source_data = SourceData {
                            source: Arc::new(source_content.clone()),
                            language: MultiCompilerLanguage::Solc(SolcLanguage::Solidity),
                            path: PathBuf::from(file_path),
                            contract_definitions: vec![(file_name, 0..source_content.len())],
                        };
                        file_map.insert(file_id as u32, Arc::new(source_data));
                    }
                } else {
                    // Fallback to single flattened source
                    let source_code = test_result
                        .source_code
                        .clone()
                        .unwrap_or_else(|| format!("// Huff test macro: {}\n// Source not available", test_result.name));

                    let source_data = SourceData {
                        source: Arc::new(source_code.clone()),
                        language: MultiCompilerLanguage::Solc(SolcLanguage::Solidity),
                        path: PathBuf::from("test.huff"),
                        contract_definitions: vec![("test".to_string(), 0..source_code.len())],
                    };
                    file_map.insert(0u32, Arc::new(source_data));
                }

                // Insert into sources_by_id with address as string key
                contracts_sources.sources_by_id.insert(address.to_string(), file_map);

                // Also need to add the artifact data with the source map
                let solidity_source_map = huff_neo_utils::artifact::SourceMapEntry::generate_solidity_source_map(source_map);

                // Parse the source map
                let parsed_source_map = foundry_compilers::artifacts::sourcemap::parse(&solidity_source_map).ok();

                if let Some(source_map_parsed) = parsed_source_map {
                    // Create PcIcMap from bytecode if available
                    let pc_ic_map = test_result.bytecode.as_ref().and_then(|bytecode_hex| {
                        alloy_primitives::hex::decode(bytecode_hex).ok().map(|bytes| foundry_evm_core::ic::PcIcMap::new(&bytes))
                    });

                    // Add artifact data for the contract
                    // The build_id should match the key in sources_by_id
                    let artifact_data = ArtifactData {
                        build_id: address.to_string(),
                        file_id: 0,
                        source_map: Some(source_map_parsed.clone()),
                        source_map_runtime: Some(source_map_parsed),
                        pc_ic_map: pc_ic_map.clone(),
                        pc_ic_map_runtime: pc_ic_map,
                    };

                    // Try adding artifacts by both name AND address
                    // Add by contract name
                    let contract_name = format!("Test::{}", test_result.name);
                    contracts_sources.artifacts_by_name.entry(contract_name).or_default().push(artifact_data.clone());

                    // Also add by address in case debugger looks it up that way
                    contracts_sources.artifacts_by_name.entry(address.to_string()).or_default().push(artifact_data);
                }
            }
        }

        debugger_builder = debugger_builder.identified_contracts(identified_contracts).sources(contracts_sources);

        let mut debugger = debugger_builder.build();

        debugger.try_run_tui().map_err(|e| RunnerError::GenericError(format!("{e:?}")))?;

        results
    }
}
