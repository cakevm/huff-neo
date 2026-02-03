use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;
use std::sync::Arc;

/// Contract source that exceeds the EIP-170 size limit (24576 bytes).
/// Uses a for loop to generate > 24576 bytes of runtime bytecode.
/// Each "0x01 pop" generates 2 bytes, so 15000 iterations = 30000 bytes.
const LARGE_CONTRACT_SOURCE: &str = r#"
    #define macro MAIN() = takes(0) returns(0) {
        // Generate a large amount of bytecode using for loop
        for(i in 0..15000) {
            0x01 pop
        }
    }
"#;

/// Small contract that is within the EIP-170 size limit.
const SMALL_CONTRACT_SOURCE: &str = r#"
    #define macro MAIN() = takes(0) returns(0) {
        0x01 0x02 add
        0x00 mstore
        0x20 0x00 return
    }
"#;

#[test]
fn test_contract_size_limit_exceeded() {
    // Lex and Parse the source code
    let flattened_source = FullFileSource { source: LARGE_CONTRACT_SOURCE, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the contract
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Generate bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Verify the bytecode exceeds the limit
    let bytecode_size = main_bytecode.len() / 2;
    assert!(
        bytecode_size > EIP170_CONTRACT_SIZE_LIMIT,
        "Test contract should exceed EIP-170 limit. Size: {} bytes, Limit: {} bytes",
        bytecode_size,
        EIP170_CONTRACT_SIZE_LIMIT
    );

    // Churn with size limit enforced - should fail
    let mut cg = Codegen::new();
    let result = cg.churn(Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false);

    assert!(result.is_err(), "churn() should fail when contract exceeds size limit");

    let err = result.unwrap_err();
    match err.kind {
        CodegenErrorKind::ContractSizeLimitExceeded(size, limit) => {
            assert_eq!(size, bytecode_size);
            assert_eq!(limit, EIP170_CONTRACT_SIZE_LIMIT);
        }
        other => panic!("Expected ContractSizeLimitExceeded error, got: {:?}", other),
    }
}

#[test]
fn test_contract_size_limit_bypassed() {
    // Lex and Parse the source code
    let flattened_source = FullFileSource { source: LARGE_CONTRACT_SOURCE, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the contract
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Generate bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Verify the bytecode exceeds the limit
    let bytecode_size = main_bytecode.len() / 2;
    assert!(
        bytecode_size > EIP170_CONTRACT_SIZE_LIMIT,
        "Test contract should exceed EIP-170 limit. Size: {} bytes, Limit: {} bytes",
        bytecode_size,
        EIP170_CONTRACT_SIZE_LIMIT
    );

    // Churn with size limit bypassed - should succeed
    let mut cg = Codegen::new();
    let result = cg.churn(Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, true);

    assert!(result.is_ok(), "churn() should succeed when no_size_limit is true");

    let artifact = result.unwrap();
    assert!(!artifact.runtime.is_empty());
    assert_eq!(artifact.runtime.len() / 2, bytecode_size);
}

#[test]
fn test_contract_within_size_limit() {
    // Lex and Parse the source code
    let flattened_source = FullFileSource { source: SMALL_CONTRACT_SOURCE, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the contract
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Generate bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Verify the bytecode is within the limit
    let bytecode_size = main_bytecode.len() / 2;
    assert!(
        bytecode_size <= EIP170_CONTRACT_SIZE_LIMIT,
        "Test contract should be within EIP-170 limit. Size: {} bytes, Limit: {} bytes",
        bytecode_size,
        EIP170_CONTRACT_SIZE_LIMIT
    );

    // Churn with size limit enforced - should succeed
    let mut cg = Codegen::new();
    let result = cg.churn(Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false);

    assert!(result.is_ok(), "churn() should succeed for contracts within size limit");
}
