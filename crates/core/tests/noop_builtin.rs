use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_noop_in_macro_generates_no_bytecode() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __NOOP
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // __NOOP should generate empty bytecode
    assert_eq!(main_bytecode, String::from(""));
}

#[test]
fn test_noop_mixed_with_opcodes() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __NOOP
            0x01
            __NOOP
            dup1
            __NOOP
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // Should only have PUSH1 0x01 and DUP1
    assert_eq!(main_bytecode, String::from("600180"));
}

#[test]
fn test_noop_as_constant() {
    let source: &str = r#"
        #define constant MY_NOOP = __NOOP

        #define macro MAIN() = takes(0) returns(0) {
            [MY_NOOP]
            0x42
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // [MY_NOOP] should generate no bytecode, only PUSH1 0x42
    assert_eq!(main_bytecode, String::from("6042"));
}

#[test]
fn test_noop_directly_in_main() {
    // Test that __NOOP can be used directly in MAIN, not as a macro argument
    // Using __NOOP as a macro argument that gets expanded with <arg> doesn't make sense
    // since it would try to reference a non-existent label
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __NOOP
            0x42
            __NOOP
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // Should only have PUSH1 0x42, no bytecode for __NOOP
    assert_eq!(main_bytecode, String::from("6042"));
}

#[test]
fn test_noop_in_constructor() {
    let source: &str = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            __NOOP
            0x01 0x00 mstore
            __NOOP
        }

        #define macro MAIN() = takes(0) returns(0) {}
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let (constructor_bytecode, _) = Codegen::generate_constructor_bytecode(evm_version, &contract, None).unwrap();

    // Should have: PUSH1 0x01, PUSH0, MSTORE
    assert_eq!(constructor_bytecode, String::from("60015f52"));
}

#[test]
fn test_noop_in_for_loop() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                __NOOP
                0x01
                __NOOP
            }
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // Loop unrolls to: 0x01 0x01 (two iterations, only the PUSH1 0x01 each time)
    assert_eq!(main_bytecode, String::from("60016001"));
}

#[test]
fn test_noop_in_label() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            my_label:
                __NOOP
                0x42
                __NOOP
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // Should have JUMPDEST (5b) + PUSH1 0x42
    assert_eq!(main_bytecode, String::from("5b6042"));
}

#[test]
fn test_rejects_noop_as_constant_name() {
    let source: &str = r#"
        #define constant __NOOP = 0x00

        #define macro MAIN() = takes(0) returns(0) {}
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, ParserErrorKind::InvalidConstantName));
}

#[test]
fn test_noop_multiple_in_sequence() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __NOOP
            __NOOP
            __NOOP
            0x01
            __NOOP
            __NOOP
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // Only PUSH1 0x01 should remain
    assert_eq!(main_bytecode, String::from("6001"));
}

#[test]
fn test_noop_standalone_usage() {
    // Test that __NOOP works in various standalone contexts
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __NOOP        // Before opcode
            dup1
            __NOOP        // After opcode
            pop
            __NOOP        // Multiple in sequence
            __NOOP
            0x01
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let evm_version = &EVMVersion::default();
    let main_bytecode = Codegen::generate_main_bytecode(evm_version, &contract, None).unwrap();

    // Should have: DUP1, POP, PUSH1 0x01 (all __NOOP instances generate nothing)
    // DUP1 = 80, POP = 50, PUSH1 0x01 = 6001
    assert_eq!(main_bytecode, String::from("80506001"));
}
