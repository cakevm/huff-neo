#![doc = include_str!("../README.md")]
#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]

use std::{collections::HashMap, sync::Arc};

use wasm_bindgen::prelude::*;

use huff_neo_core::Compiler;
use huff_neo_utils::{
    abi::Abi,
    artifact::{self, Artifact},
    prelude::EVMVersion,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct CompilerInput {
    evm_version: Option<String>,
    sources: Vec<String>,
    files: HashMap<String, String>,
    construct_args: Option<Vec<String>>,
    alternative_main: Option<String>,
    alternative_constructor: Option<String>,
}

#[derive(Serialize, Deserialize)]
struct CompilerArtifact {
    bytecode: String,
    runtime: String,
    abi: Option<Abi>,
    constructor_map: Option<Vec<artifact::SourceMapEntry>>,
    runtime_map: Option<Vec<artifact::SourceMapEntry>>,
}

#[derive(Serialize, Deserialize)]
struct CompilerOutput {
    errors: Option<Vec<String>>,
    contracts: Option<HashMap<String, CompilerArtifact>>,
}

/// Compiles contracts based on supplied JSON input
#[wasm_bindgen]
pub fn compile(input: JsValue) -> Result<JsValue, JsValue> {
    let input: CompilerInput = match serde_wasm_bindgen::from_value(input) {
        Ok(inp) => inp,
        Err(err) => {
            // Return deserialization error as part of the output
            let output = CompilerOutput { errors: Some(vec![format!("Failed to parse input: {}", err)]), contracts: None };
            return serde_wasm_bindgen::to_value(&output).map_err(|_| JsValue::NULL);
        }
    };

    let evm_version = EVMVersion::from(input.evm_version);

    let compiler = Compiler::new_in_memory(
        &evm_version,
        Arc::new(input.sources.clone()),
        input.files.clone(),
        input.alternative_main.clone(),
        input.alternative_constructor.clone(),
        input.construct_args.clone(),
        None,
        false,
    );

    // Handle compilation errors gracefully by returning them in the output
    let res: Vec<Arc<Artifact>> = match compiler.execute() {
        Ok(artifacts) => artifacts,
        Err(err) => {
            // Return compilation errors as part of the output, not as a JS error
            let output = CompilerOutput { errors: Some(vec![format!("{}", *err)]), contracts: None };
            return serde_wasm_bindgen::to_value(&output).map_err(|_| JsValue::NULL);
        }
    };

    // If no artifacts generated, return a debug error
    if res.is_empty() {
        let output = CompilerOutput {
            errors: Some(vec!["No artifacts generated. Check that files are accessible and contain valid Huff code.".to_string()]),
            contracts: None,
        };
        return serde_wasm_bindgen::to_value(&output).map_err(|_| JsValue::NULL);
    }

    let mut contracts: HashMap<String, CompilerArtifact> = HashMap::new();

    res.into_iter().for_each(|artifact| {
        contracts.insert(
            artifact.file.path.clone(),
            CompilerArtifact {
                bytecode: artifact.bytecode.clone(),
                runtime: artifact.runtime.clone(),
                abi: artifact.abi.clone(),
                constructor_map: artifact.constructor_map.clone(),
                runtime_map: artifact.runtime_map.clone(),
            },
        );
    });

    let output = CompilerOutput { errors: None, contracts: Some(contracts) };

    serde_wasm_bindgen::to_value(&output).map_err(|_| JsValue::NULL)
}
