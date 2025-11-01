/// Tests for EIP-7939 CLZ opcode and EVM version validation
use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_clz_compiles_with_osaka() {
    // Test that CLZ compiles successfully with Osaka EVM version
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x01
            clz
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Validate opcodes with Osaka version - should succeed
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "CLZ should validate successfully with Osaka version");

    // Generate bytecode to confirm CLZ (0x1e) is present
    let bytecode_result = Codegen::generate_main_bytecode(&evm_version, &contract, None);
    assert!(bytecode_result.is_ok());
    let bytecode = bytecode_result.unwrap();

    // Bytecode should contain: 6001 (PUSH1 0x01), 1e (CLZ), 50 (POP), 00 (STOP)
    assert!(bytecode.contains("1e"), "Bytecode should contain CLZ opcode (0x1e)");
}

#[test]
fn test_clz_fails_with_prague() {
    // Test that CLZ fails to validate with Prague (pre-Osaka) EVM version
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x01
            clz
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Validate opcodes with Prague version - should fail
    let evm_version = EVMVersion::new(SupportedEVMVersions::Prague);
    let validation_result = contract.validate_opcodes(&evm_version);

    assert!(validation_result.is_err(), "CLZ should fail validation with Prague version");
    let error = validation_result.unwrap_err();

    // Verify error is about invalid opcode for EVM version
    assert!(matches!(error.kind, CodegenErrorKind::InvalidOpcodeForEVMVersion(_, _, _)));

    if let CodegenErrorKind::InvalidOpcodeForEVMVersion(opcode, required, current) = error.kind {
        assert_eq!(opcode, "clz", "Error should reference CLZ opcode");
        assert_eq!(required, "osaka", "Error should indicate Osaka is required");
        assert_eq!(current, "prague", "Error should indicate current version is Prague");
    }
}

#[test]
fn test_clz_fails_with_cancun() {
    // Test that CLZ fails with even older versions (Cancun)
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0xFF
            clz
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::new(SupportedEVMVersions::Cancun);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "CLZ should fail validation with Cancun version");
}

#[test]
fn test_push0_validation() {
    // Test that PUSH0 opcode validation works correctly
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            push0
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // PUSH0 should fail with Paris
    let evm_version = EVMVersion::new(SupportedEVMVersions::Paris);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "PUSH0 should fail validation with Paris version");

    // PUSH0 should succeed with Shanghai
    let evm_version = EVMVersion::new(SupportedEVMVersions::Shanghai);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "PUSH0 should validate successfully with Shanghai version");
}

#[test]
fn test_tload_validation() {
    // Test that Cancun opcodes (TLOAD) are validated
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x00
            tload
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // TLOAD should fail with Shanghai
    let evm_version = EVMVersion::new(SupportedEVMVersions::Shanghai);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "TLOAD should fail validation with Shanghai version");

    // TLOAD should succeed with Cancun
    let evm_version = EVMVersion::new(SupportedEVMVersions::Cancun);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "TLOAD should validate successfully with Cancun version");
}

#[test]
fn test_mcopy_validation() {
    // Test that MCOPY opcode is validated
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x20 0x00 0x00
            mcopy
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // MCOPY should fail with Shanghai
    let evm_version = EVMVersion::new(SupportedEVMVersions::Shanghai);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "MCOPY should fail validation with Shanghai version");

    // MCOPY should succeed with Cancun
    let evm_version = EVMVersion::new(SupportedEVMVersions::Cancun);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "MCOPY should validate successfully with Cancun version");
}

#[test]
fn test_clz_in_nested_macro_invocation() {
    // Test that CLZ is validated even when inside a nested macro invocation
    let source = r#"
        #define macro HELPER() = takes(0) returns(1) {
            0x01
            clz
        }

        #define macro MAIN() = takes(0) returns(0) {
            HELPER()
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Should succeed with Osaka
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "CLZ in nested macro should validate with Osaka");

    // Should fail with Prague
    let evm_version = EVMVersion::new(SupportedEVMVersions::Prague);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "CLZ in nested macro should fail validation with Prague");
}

#[test]
fn test_clz_in_label() {
    // Test that CLZ is validated when inside a label definition
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            my_label:
                0xFF
                clz
                pop
            0x00
            my_label
            jumpi
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Should succeed with Osaka
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "CLZ in label should validate with Osaka");

    // Should fail with Prague
    let evm_version = EVMVersion::new(SupportedEVMVersions::Prague);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "CLZ in label should fail validation with Prague");
}

#[test]
fn test_clz_in_deeply_nested_macro() {
    // Test that CLZ is validated in deeply nested macro invocations
    let source = r#"
        #define macro INNER() = takes(0) returns(1) {
            0x42
            clz
        }

        #define macro MIDDLE() = takes(0) returns(1) {
            INNER()
        }

        #define macro MAIN() = takes(0) returns(0) {
            MIDDLE()
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Should succeed with Osaka
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "CLZ in deeply nested macro should validate with Osaka");

    // Should fail with Prague
    let evm_version = EVMVersion::new(SupportedEVMVersions::Prague);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "CLZ in deeply nested macro should fail with Prague");

    // Verify the error details
    if let Err(error) = validation_result {
        assert!(matches!(error.kind, CodegenErrorKind::InvalidOpcodeForEVMVersion(_, _, _)));
        if let CodegenErrorKind::InvalidOpcodeForEVMVersion(opcode, required, current) = error.kind {
            assert_eq!(opcode, "clz", "Error should reference CLZ opcode");
            assert_eq!(required, "osaka", "Error should indicate Osaka is required");
            assert_eq!(current, "prague", "Error should indicate current version is Prague");
        }
    }
}

#[test]
fn test_multiple_version_specific_opcodes() {
    // Test that multiple version-specific opcodes are all validated
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            push0         // Shanghai
            0x00
            tload         // Cancun
            0x01
            clz           // Osaka
            pop
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Should fail with Paris (no push0, tload, or clz)
    let evm_version = EVMVersion::new(SupportedEVMVersions::Paris);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "Should fail with Paris due to push0");

    // Should fail with Shanghai (has push0, but no tload or clz)
    let evm_version = EVMVersion::new(SupportedEVMVersions::Shanghai);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "Should fail with Shanghai due to tload");

    // Should fail with Cancun (has push0 and tload, but no clz)
    let evm_version = EVMVersion::new(SupportedEVMVersions::Cancun);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "Should fail with Cancun due to clz");

    // Should succeed with Osaka (has all opcodes)
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "Should succeed with Osaka");
}

#[test]
fn test_clz_passed_as_macro_argument() {
    // Test that CLZ is validated when passed as a macro argument
    let source = r#"
        #define macro APPLY_OP(op) = takes(1) returns(1) {
            <op>
        }

        #define macro MAIN() = takes(0) returns(0) {
            0x42
            APPLY_OP(clz)
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Should succeed with Osaka
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "CLZ as macro argument should validate with Osaka");

    // Should fail with Prague
    let evm_version = EVMVersion::new(SupportedEVMVersions::Prague);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "CLZ as macro argument should fail validation with Prague");

    // Verify the error details
    if let Err(error) = validation_result {
        assert!(matches!(error.kind, CodegenErrorKind::InvalidOpcodeForEVMVersion(_, _, _)));
        if let CodegenErrorKind::InvalidOpcodeForEVMVersion(opcode, required, current) = error.kind {
            assert_eq!(opcode, "clz", "Error should reference CLZ opcode");
            assert_eq!(required, "osaka", "Error should indicate Osaka is required");
            assert_eq!(current, "prague", "Error should indicate current version is Prague");
        }
    }
}

#[test]
fn test_multiple_opcodes_as_arguments() {
    // Test multiple version-specific opcodes passed as arguments
    let source = r#"
        #define macro USE_TWO_OPS(op1, op2) = takes(2) returns(1) {
            <op1>
            <op2>
        }

        #define macro MAIN() = takes(0) returns(0) {
            0x00
            0x01
            USE_TWO_OPS(tload, clz)
            pop
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Should fail with Paris (no tload or clz)
    let evm_version = EVMVersion::new(SupportedEVMVersions::Paris);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "Should fail with Paris");

    // Should fail with Cancun (has tload but no clz)
    let evm_version = EVMVersion::new(SupportedEVMVersions::Cancun);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_err(), "Should fail with Cancun due to clz");

    // Should succeed with Osaka
    let evm_version = EVMVersion::new(SupportedEVMVersions::Osaka);
    let validation_result = contract.validate_opcodes(&evm_version);
    assert!(validation_result.is_ok(), "Should succeed with Osaka");
}
