use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::prelude::*;

/// Test that jump relaxation reduces bytecode size for small contracts
#[test]
fn test_jump_relaxation_small_contract() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            start:
                0x01
                dup1
                end jump
            end:
                0x02
                0x03
                add
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    assert_eq!(bytecode_without.len() - bytecode_with.len(), 2, "Relaxation should reduce bytecode by 1 byte");
}

/// Test forward jumps
#[test]
fn test_jump_relaxation_forward_jump() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            forward jump
            0x01
            0x02
            forward:
                0x00
                mstore
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // With relaxation, should use PUSH1 (60) instead of PUSH2 (61)
    assert!(bytecode_with.len() < bytecode_without.len(), "Forward jump should be optimized");
    assert_eq!(bytecode_with, "600856600160025b5f52", "Should use PUSH1 for near jump");
}

/// Test backward jumps
#[test]
fn test_jump_relaxation_backward_jump() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            loop:
                0x01
                0x01
                add
                dup1
                loop jump
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Backward jump should be optimized to PUSH1
    assert!(bytecode_with.len() < bytecode_without.len(), "Backward jump should be optimized");
    assert_eq!(bytecode_with, "5b600160010180600056", "Should use PUSH1 for backward jump");
}

/// Test multiple jumps
#[test]
fn test_jump_relaxation_multiple_jumps() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            label1 jump
            label1:
                0x01
                label2 jump
            label2:
                0x02
                label3 jump
            label3:
                0x03
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // All three jumps should be optimized
    assert!(bytecode_with.len() < bytecode_without.len(), "Multiple jumps should be optimized");
    assert_eq!(bytecode_with, "6004565b6001600b565b60026012565b6003", "All jumps should use PUSH1");
}

/// Test for loop creating large gap - jump should NOT be optimized
#[test]
fn test_jump_relaxation_large_gap_no_optimization() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            end jump
            for(i in 0..50) {
                0x01
                0x02
                add
                pop
            }
            end:
                0xff
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Large gap means jump target > 255, so PUSH2 must be preserved
    assert_eq!(bytecode_without.len(), bytecode_with.len(), "Jump over large gap should NOT be optimized");
    // Verify bytecode contains PUSH2 (0x61) at the start for the jump
    assert!(bytecode_with.starts_with("61"), "Should still use PUSH2 for far jump");
}

/// Test cross jumps - mixed optimization with PUSH1 and PUSH2
#[test]
fn test_jump_relaxation_cross_jumps_mixed() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            near jump
            0x01 0x01 0x01
            near:
                0x00
                far jump
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
            far:
                0xaa
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Near jump should be optimized to PUSH1, far jump should stay PUSH2
    assert!(bytecode_with.len() < bytecode_without.len(), "Near jump should be optimized even with far jump present");
}

/// Test multiple cross jumps with for loops
#[test]
fn test_jump_relaxation_multiple_cross_jumps() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            label1 jump
            for(i in 0..5) {
                0x02
            }
            label1:
                0x01
                label2 jump
            for(i in 0..5) {
                0x03
            }
            label2:
                0x04
                label3 jump
            for(i in 0..5) {
                0x05
            }
            label3:
                0x06
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // All jumps should still be optimizable with small for loops
    assert!(bytecode_with.len() < bytecode_without.len(), "Jumps with small for loop gaps should be optimized");
}
