#![allow(dead_code)]

use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::error::{CodegenError, CodegenErrorKind};
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;
use std::sync::Arc;

/// Compile a Huff source string and return the main bytecode
pub fn compile_to_bytecode(source: &str) -> Result<String, CodegenError> {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();
    Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false)
}

/// Compile a Huff source string and return the constructor bytecode
pub fn compile_to_constructor_bytecode(source: &str) -> Result<(String, bool), CodegenError> {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();
    Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None, false)
}

/// Compile a Huff source string and return the full deployment artifact (constructor body +
/// bootstrap + main + constructor-only tables + args).
pub fn compile_to_deployment(source: &str) -> String {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm = &EVMVersion::default();
    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, false).unwrap();
    let (constructor, updated_contract) = match Codegen::generate_constructor_macro_bytecode(evm, &contract, None, false) {
        Ok((ctor, updated)) => (Some(ctor), updated),
        Err(_) => (None, Codegen::update_table_size(evm, &contract).unwrap()),
    };

    let mut cg = Codegen::new();
    let artifact = cg.assemble_artifact(Arc::new(FileSource::default()), &updated_contract, vec![], main, constructor, false).unwrap();
    artifact.bytecode
}

/// Compile a Huff source and assert it produces an error of the expected kind
pub fn assert_compile_error(source: &str, expected_kind: fn(&CodegenErrorKind) -> bool) {
    let result = compile_to_bytecode(source);
    assert!(result.is_err(), "Expected compilation to fail but it succeeded");
    if let Err(e) = result {
        assert!(expected_kind(&e.kind), "Expected error kind mismatch. Got: {:?}", e.kind);
    }
}
