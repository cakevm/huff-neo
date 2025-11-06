use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::prelude::*;

#[test]
fn test_simple_macro_as_argument() {
    // Test passing a macro as an argument to another macro
    let source = r#"
        #define macro ADD() = takes(2) returns(1) {
            add
        }

        #define macro APPLY_OP(op) = takes(0) returns(1) {
            0x10
            0x20
            <op>()  // Call the macro passed as argument
        }

        #define macro MAIN() = takes(0) returns(0) {
            APPLY_OP(ADD)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected bytecode: PUSH1 0x10, PUSH1 0x20, ADD
    // 60 10 60 20 01
    assert_eq!(bytecode, "6010602001");
}

#[test]
fn test_multiple_macro_arguments() {
    // Test passing multiple macros as arguments
    let source = r#"
        #define macro ADD() = takes(2) returns(1) {
            add
        }

        #define macro MUL() = takes(2) returns(1) {
            mul
        }

        #define macro APPLY_TWO_OPS(op1, op2) = takes(0) returns(2) {
            0x10
            0x20
            <op1>()  // First operation
            0x30
            0x40
            <op2>()  // Second operation
        }

        #define macro MAIN() = takes(0) returns(0) {
            APPLY_TWO_OPS(ADD, MUL)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected bytecode: PUSH1 0x10, PUSH1 0x20, ADD, PUSH1 0x30, PUSH1 0x40, MUL
    // 60 10 60 20 01 60 30 60 40 02
    assert_eq!(bytecode, "60106020016030604002");
}

#[test]
fn test_macro_arg_with_parameters() {
    // Test passing macros that themselves take arguments
    let source = r#"
        #define macro ADD_CONST(val) = takes(1) returns(1) {
            <val>
            add
        }

        #define macro APPLY_WITH_CONST(op) = takes(0) returns(1) {
            0x10
            <op>(0x05)  // Pass argument to the macro argument
        }

        #define macro MAIN() = takes(0) returns(0) {
            APPLY_WITH_CONST(ADD_CONST)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected bytecode: PUSH1 0x10, PUSH1 0x05, ADD
    // 60 10 60 05 01
    assert_eq!(bytecode, "6010600501");
}

#[test]
fn test_nested_macro_as_arg() {
    // Test nested usage of macros as arguments
    let source = r#"
        #define macro ADD() = takes(2) returns(1) {
            add
        }

        #define macro WRAPPER(inner_op) = takes(0) returns(1) {
            0x01
            0x02
            <inner_op>()
        }

        #define macro OUTER(op) = takes(0) returns(1) {
            WRAPPER(<op>)  // Pass the macro argument to another macro
        }

        #define macro MAIN() = takes(0) returns(0) {
            OUTER(ADD)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected bytecode: PUSH1 0x01, PUSH1 0x02, ADD
    // 60 01 60 02 01
    assert_eq!(bytecode, "6001600201");
}

#[test]
fn test_conditional_macro_selection() {
    // Test selecting different macros based on conditions
    let source = r#"
        #define macro ADD() = takes(2) returns(1) {
            add
        }

        #define macro SUB() = takes(2) returns(1) {
            0x03 sub
        }

        #define macro MUL() = takes(2) returns(1) {
            mul
        }

        #define macro COMPUTE(op1, op2) = takes(0) returns(2) {
            // First computation
            0x10
            0x05
            <op1>()
            
            // Second computation  
            0x20
            0x04
            <op2>()
        }

        #define macro MAIN() = takes(0) returns(0) {
            COMPUTE(ADD, MUL)  // Add first, then multiply
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected bytecode: PUSH1 0x10, PUSH1 0x05, ADD, PUSH1 0x20, PUSH1 0x04, MUL
    // 60 10 60 05 01 60 20 60 04 02
    assert_eq!(bytecode, "60106005016020600402");
}
