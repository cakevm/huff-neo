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
        bytecode_size > EIP170_MAX_CODE_SIZE,
        "Test contract should exceed EIP-170 limit. Size: {} bytes, Limit: {} bytes",
        bytecode_size,
        EIP170_MAX_CODE_SIZE
    );

    // Churn with size limit enforced - should fail
    let mut cg = Codegen::new();
    let result = cg.churn(&EVMVersion::default(), Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false);

    assert!(result.is_err(), "churn() should fail when contract exceeds size limit");

    let err = result.unwrap_err();
    match err.kind {
        CodegenErrorKind::ContractSizeLimitExceeded(size, limit) => {
            assert_eq!(size, bytecode_size);
            assert_eq!(limit, EIP170_MAX_CODE_SIZE);
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
        bytecode_size > EIP170_MAX_CODE_SIZE,
        "Test contract should exceed EIP-170 limit. Size: {} bytes, Limit: {} bytes",
        bytecode_size,
        EIP170_MAX_CODE_SIZE
    );

    // Churn with size limit bypassed - should succeed
    let mut cg = Codegen::new();
    let result = cg.churn(&EVMVersion::default(), Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, true);

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
        bytecode_size <= EIP170_MAX_CODE_SIZE,
        "Test contract should be within EIP-170 limit. Size: {} bytes, Limit: {} bytes",
        bytecode_size,
        EIP170_MAX_CODE_SIZE
    );

    // Churn with size limit enforced - should succeed
    let mut cg = Codegen::new();
    let result = cg.churn(&EVMVersion::default(), Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false);

    assert!(result.is_ok(), "churn() should succeed for contracts within size limit");
}

/// Compiles the large contract (above the EIP-170 limit) to runtime bytecode.
fn large_contract_bytecode() -> String {
    let flattened_source = FullFileSource { source: LARGE_CONTRACT_SOURCE, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();
    Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap()
}

#[test]
fn test_amsterdam_allows_contracts_above_eip170() {
    let main_bytecode = large_contract_bytecode();
    assert!(main_bytecode.len() / 2 > EIP170_MAX_CODE_SIZE);

    let amsterdam = EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let mut cg = Codegen::new();
    let result = cg.churn(&amsterdam, Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false);
    assert!(result.is_ok(), "Amsterdam raises the runtime limit to 64 KiB (EIP-7954)");
}

#[test]
fn test_amsterdam_rejects_contracts_above_eip7954() {
    let main_bytecode = "00".repeat(EIP7954_MAX_CODE_SIZE + 1);

    let amsterdam = EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let mut cg = Codegen::new();
    let err = cg.churn(&amsterdam, Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false).unwrap_err();
    match err.kind {
        CodegenErrorKind::ContractSizeLimitExceeded(size, limit) => {
            assert_eq!(size, EIP7954_MAX_CODE_SIZE + 1);
            assert_eq!(limit, EIP7954_MAX_CODE_SIZE);
        }
        other => panic!("Expected ContractSizeLimitExceeded error, got: {:?}", other),
    }
}

#[test]
fn test_max_size_runtime_uses_push3_bootstrap() {
    // A runtime of exactly 64 KiB is valid from Amsterdam, but its length (0x010000) no longer
    // fits into PUSH2, so the bootstrap must widen to PUSH3.
    let main_bytecode = "00".repeat(EIP7954_MAX_CODE_SIZE);

    let amsterdam = EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let mut cg = Codegen::new();
    let artifact = cg.churn(&amsterdam, Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false).unwrap();

    // PUSH3 0x010000, DUP1, PUSH1 0x0b, RETURNDATASIZE, CODECOPY, RETURNDATASIZE, RETURN
    let bootstrap = "6201000080600b3d393df3";
    assert!(artifact.bytecode.starts_with(bootstrap), "unexpected bootstrap: {}", &artifact.bytecode[..30]);
    assert_eq!(artifact.bytecode.len() / 2, bootstrap.len() / 2 + EIP7954_MAX_CODE_SIZE);
}

#[test]
fn test_initcode_size_limit() {
    // Runtime stays within EIP-170, but constructor + runtime exceed the EIP-3860 initcode limit
    let constructor_bytecode = "00".repeat(30_000);
    let main_bytecode = "00".repeat(20_000);

    let mut cg = Codegen::new();
    let err = cg
        .churn(
            &EVMVersion::default(),
            Arc::new(FileSource::default()),
            vec![],
            &main_bytecode,
            &constructor_bytecode,
            false,
            None,
            None,
            false,
        )
        .unwrap_err();
    match err.kind {
        CodegenErrorKind::InitcodeSizeLimitExceeded(size, limit) => {
            assert!(size > EIP3860_MAX_INITCODE_SIZE);
            assert_eq!(limit, EIP3860_MAX_INITCODE_SIZE);
        }
        other => panic!("Expected InitcodeSizeLimitExceeded error, got: {:?}", other),
    }

    // The same deployment is fine under Amsterdam (128 KiB initcode, EIP-7954)
    let amsterdam = EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let mut cg = Codegen::new();
    let result =
        cg.churn(&amsterdam, Arc::new(FileSource::default()), vec![], &main_bytecode, &constructor_bytecode, false, None, None, false);
    assert!(result.is_ok());

    // And it can be bypassed explicitly
    let mut cg = Codegen::new();
    let result = cg.churn(
        &EVMVersion::default(),
        Arc::new(FileSource::default()),
        vec![],
        &main_bytecode,
        &constructor_bytecode,
        false,
        None,
        None,
        true,
    );
    assert!(result.is_ok());
}

#[test]
fn test_max_size_runtime_jumps_to_last_byte() {
    // In a runtime of exactly 64 KiB (the Amsterdam limit), the last byte sits at 0xffff, so every
    // jump target still fits into PUSH2.
    // PUSH2 target, JUMP (4 bytes) + 65531 STOPs + JUMPDEST at 0xffff = 65536 bytes
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            target jump
            for(i in 0..65531) { stop }
            target:
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let amsterdam = EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let main_bytecode = Codegen::generate_main_bytecode(&amsterdam, &contract, None, false).unwrap();
    assert_eq!(main_bytecode.len() / 2, EIP7954_MAX_CODE_SIZE);
    assert!(main_bytecode.starts_with("61ffff56"), "jump must target 0xffff via PUSH2");
    assert!(main_bytecode.ends_with("005b"), "last byte must be the JUMPDEST");

    let mut cg = Codegen::new();
    let result = cg.churn(&amsterdam, Arc::new(FileSource::default()), vec![], &main_bytecode, "", false, None, None, false);
    assert!(result.is_ok(), "a 64 KiB runtime is within the Amsterdam limit");
}
