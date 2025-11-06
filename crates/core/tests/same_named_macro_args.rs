use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

/// Tests for macro argument scoping with same-named parameters
///
/// These tests verify that arguments with the same names across different macros
/// are properly resolved without causing infinite recursion or stack overflow.

#[test]
fn test_same_named_args_simple() {
    // Simple case with same-named parameters across two macros
    let source = r#"
        #define macro ADD(a, b) = takes(2) returns(1) {
            <b> <a> add
        }

        #define macro MUL(a, b) = takes(2) returns(1) {
            <b> <a> mul
        }

        #define macro RUN(x, y) = takes(0) returns(1) {
            MUL(ADD(<x>, <y>), 0x02)
        }

        #define macro MAIN() = takes(0) returns(1) {
            RUN(0x01, 0x03)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // MUL processes <b> first (0x02), then <a> (which evaluates ADD)
    // Result: PUSH1 0x02, PUSH1 0x03, PUSH1 0x01, ADD, MUL
    assert_eq!(main_bytecode.to_lowercase(), "6002600360010102");
}

#[test]
fn test_same_named_args_complex_nesting() {
    let source = r#"
        #define macro ADD(a, b) = {
            <b>
            <a>
            add
        }

        #define macro EQUAL(a, b) = {
            <b>
            <a>
            eq
        }

        #define macro RUN(a, b, c) = {
            EQUAL(ADD(<a>, <b>), <c>)
        }

        #define macro MAIN() = {
            RUN(0x01, 0x02, 0x03)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // EQUAL processes <b> first (0x03), then <a> (which evaluates ADD)
    // Result: PUSH1 0x03, PUSH1 0x02, PUSH1 0x01, ADD, EQ
    assert_eq!(main_bytecode.to_lowercase(), "6003600260010114");
}

#[test]
fn test_different_named_args_equivalent() {
    // Equivalent case with different parameter names - should produce same result
    let source = r#"
        #define macro ADD(a1, b1) = {
            <b1>
            <a1>
            add
        }

        #define macro EQUAL(a2, b2) = {
            <b2>
            <a2>
            eq
        }

        #define macro RUN(a3, b3, c3) = {
            EQUAL(ADD(<a3>, <b3>), <c3>)
        }

        #define macro MAIN() = {
            RUN(0x01, 0x02, 0x03)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Should produce the exact same bytecode as the same-named version
    assert_eq!(main_bytecode.to_lowercase(), "6003600260010114");
}

#[test]
fn test_mixed_same_and_different_names() {
    // Mix of same and different parameter names
    let source = r#"
        #define macro LOAD(val) = takes(0) returns(1) {
            <val>
        }

        #define macro ADD(a, b) = takes(2) returns(1) {
            <b> <a> add
        }

        #define macro SUB(a, b) = takes(2) returns(1) {
            <a> <b> sub
        }

        #define macro PROCESS(x, y, z) = takes(0) returns(1) {
            SUB(ADD(LOAD(<x>), LOAD(<y>)), LOAD(<z>))
        }

        #define macro MAIN() = takes(0) returns(1) {
            PROCESS(0x10, 0x05, 0x02)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // SUB(<a>, <b>) processes <a> first (ADD expression), then <b> (LOAD(0x02))
    // Result: PUSH1 0x05, PUSH1 0x10, ADD, PUSH1 0x02, SUB
    assert_eq!(main_bytecode.to_lowercase(), "6005601001600203");
}

#[test]
fn test_deeply_nested_same_names() {
    // Deep nesting with same parameter names at multiple levels
    let source = r#"
        #define macro INNERMOST(a, b) = takes(2) returns(1) {
            <a> <b> add
        }

        #define macro MIDDLE(a, b) = takes(0) returns(1) {
            INNERMOST(<a>, <b>)
        }

        #define macro OUTER(a, b) = takes(0) returns(1) {
            MIDDLE(<a>, <b>)
        }

        #define macro MAIN() = takes(0) returns(1) {
            OUTER(0x07, 0x08)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x07, PUSH1 0x08, ADD
    assert_eq!(main_bytecode.to_lowercase(), "6007600801");
}
