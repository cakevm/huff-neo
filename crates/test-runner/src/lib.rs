use crate::{errors::RunnerError, types::TestResult};
use huff_neo_utils::prelude::{Contract, MacroDefinition, SupportedEVMVersions};

/// The runner module
pub mod runner;

/// The report module
pub mod report;

/// The types module
pub mod types;

/// The errors module
pub mod errors;

/// The decorator spec module
pub mod spec;

/// The test outcome module
pub mod outcome;

/// The module that shows Huff functions as internal calls in traces
pub mod internal;

use alloy_primitives::map::AddressHashMap;
use foundry_compilers::{compilers::solc::SolcLanguage, multi::MultiCompilerLanguage};
use foundry_debugger::Debugger;
use foundry_evm::traces::TraceKind;
use foundry_evm_traces::debug::{ArtifactData, ContractSources, SourceData};
use revm::primitives::hardfork::SpecId;
use std::path::PathBuf;
use std::sync::Arc;

pub use crate::runner::{Env, TestRunner, cheats_config};
pub use foundry_evm::inspectors::CheatsConfig;
pub use revm::context::TxEnv;

/// Prelude wraps all modules within the crate
pub mod prelude {
    pub use crate::{errors::*, internal::*, outcome::*, report::*, runner::*, spec::*, types::*};
}

/// A vector of shared references to test macro definitions
pub type TestMacros<'t> = Vec<&'t MacroDefinition>;

/// Returns the REVM hardfork that matches a Huff EVM version
pub fn spec_id_for(version: &SupportedEVMVersions) -> SpecId {
    match version {
        SupportedEVMVersions::Paris => SpecId::MERGE,
        SupportedEVMVersions::Shanghai => SpecId::SHANGHAI,
        SupportedEVMVersions::Cancun => SpecId::CANCUN,
        SupportedEVMVersions::Prague => SpecId::PRAGUE,
        SupportedEVMVersions::Osaka => SpecId::OSAKA,
        SupportedEVMVersions::Amsterdam => SpecId::AMSTERDAM,
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

    /// Whether to open the single matched test in the debugger after running it
    pub debug: bool,
}

/// HuffTester implementation
impl<'t> HuffTester<'t> {
    /// Create a new instance of `HuffTester` from a contract's AST.
    pub fn new(ast: &'t Contract, match_test_name: Option<String>, runner: TestRunner, debug: bool) -> Self {
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
            runner,
            debug,
        }
    }

    /// Execute tests
    pub fn execute(self) -> Result<Vec<TestResult>, RunnerError> {
        // Check if any test macros exist
        if self.macros.is_empty() {
            return Err(RunnerError::GenericError(String::from("No test macros found.")));
        }

        // Execute our tests and return a vector of the results
        let results = self
            .macros
            .into_iter()
            .map(|macro_def| {
                // Every test gets its own executor; each one must fork the same chain state
                let mut executor = self.runner.new_executor()?;
                self.runner.run_test(&mut executor, macro_def, self.ast)
            })
            .collect::<Result<Vec<TestResult>, RunnerError>>();

        if !self.debug {
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

        let Some(arena) = test_result.traces.clone() else {
            return Err(RunnerError::GenericError("No trace found".to_string()));
        };

        let traces = [(TraceKind::Execution, arena)];

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
                            debug_scopes: Vec::new(),
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
                        debug_scopes: Vec::new(),
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
