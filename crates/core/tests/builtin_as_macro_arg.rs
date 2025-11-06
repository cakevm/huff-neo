use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_builtin_function_as_macro_argument() {
    // Test that builtin functions can be passed as macro arguments (issue #130)
    let source = r#"
        #define function transfer(address,uint256) nonpayable returns ()

        #define macro PUSH_VALUE(val) = takes(0) returns(1) {
            <val>
        }

        #define macro MAIN() = takes(0) returns(0) {
            PUSH_VALUE(__FUNC_SIG(transfer))
            pop
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen
    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);
    assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

    let bytecode = result.unwrap();

    // Verify the bytecode contains the function selector for transfer(address,uint256)
    // The selector is 0xa9059cbb
    assert!(bytecode.contains("63a9059cbb"), "Bytecode should contain function selector for transfer: {}", bytecode);
}

#[test]
fn test_event_hash_as_macro_argument() {
    // Test that event hash builtin can be passed as macro argument
    let source = r#"
        #define event Transfer(address indexed,address indexed,uint256)

        #define macro USE_HASH(hash) = takes(0) returns(1) {
            <hash>
        }

        #define macro MAIN() = takes(0) returns(0) {
            USE_HASH(__EVENT_HASH(Transfer))
            pop
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen
    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);
    assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

    let bytecode = result.unwrap();

    // Should successfully generate bytecode with event hash
    assert!(!bytecode.is_empty(), "Bytecode should not be empty");
}

#[test]
fn test_nested_builtin_calls_as_macro_arguments() {
    // Test more complex nesting: passing builtin to a macro that passes it to another macro
    let source = r#"
        #define function balanceOf(address) view returns (uint256)

        #define macro INNER(sig) = takes(0) returns(1) {
            <sig>
        }

        #define macro OUTER(value) = takes(0) returns(1) {
            INNER(<value>)
        }

        #define macro MAIN() = takes(0) returns(0) {
            OUTER(__FUNC_SIG(balanceOf))
            pop
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen
    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);
    assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

    let bytecode = result.unwrap();

    // Verify the bytecode contains the function selector for balanceOf(address)
    // The selector is 0x70a08231
    assert!(bytecode.contains("6370a08231"), "Bytecode should contain function selector for balanceOf: {}", bytecode);
}

#[test]
fn test_bytes_builtin_as_macro_argument() {
    // Test that __BYTES can be passed as macro argument
    let source = r#"
        #define macro PUSH_VALUE(val) = takes(0) returns(1) {
            <val>
        }

        #define macro MAIN() = takes(0) returns(0) {
            PUSH_VALUE(__BYTES("test"))
            pop
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen
    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);
    assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

    let bytecode = result.unwrap();

    // Verify the bytecode contains the UTF-8 bytes for "test"
    // "test" = 0x74657374
    assert!(bytecode.contains("74657374"), "Bytecode should contain bytes for 'test': {}", bytecode);
}

#[test]
fn test_rightpad_builtin_as_macro_argument() {
    // Test that __RIGHTPAD can be passed as macro argument
    let source = r#"
        #define macro PUSH_VALUE(val) = takes(0) returns(1) {
            <val>
        }

        #define macro MAIN() = takes(0) returns(0) {
            PUSH_VALUE(__RIGHTPAD(0x1234))
            pop
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen
    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);
    assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

    let bytecode = result.unwrap();

    // Verify the bytecode contains the right-padded value
    // Should be padded to 32 bytes: 0x1234...0000
    assert!(bytecode.contains("1234"), "Bytecode should contain right-padded value: {}", bytecode);
}
