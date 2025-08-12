use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::error::CodegenErrorKind;
use huff_neo_utils::prelude::*;

#[test]
fn test_direct_circular_recursion_in_codegen() {
    // Direct circular recursion: MAIN calls itself
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            MAIN()
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    // derive_storage_pointers should handle circular recursion gracefully
    contract.derive_storage_pointers(); // Should not panic anymore

    // The actual codegen should also detect circular recursion
    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular macro invocation error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, CodegenErrorKind::CircularMacroInvocation("MAIN".to_string()));
}

#[test]
fn test_indirect_circular_recursion() {
    // Indirect circular recursion: A -> B -> A
    let source = r#"
        #define macro A() = takes(0) returns(0) {
            B()
        }
        
        #define macro B() = takes(0) returns(0) {
            A()
        }
        
        #define macro MAIN() = takes(0) returns(0) {
            A()
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    // derive_storage_pointers should handle circular recursion gracefully
    contract.derive_storage_pointers(); // Should not panic anymore

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular macro invocation error
    assert!(result.is_err());
    let err = result.unwrap_err();
    // The error will be for whichever macro is detected as circular first (either A or B)
    assert!(matches!(err.kind, CodegenErrorKind::CircularMacroInvocation(_)));
}

#[test]
fn test_valid_non_circular_recursion() {
    // Valid case: macro with conditional termination (simulated with different macros)
    let source = r#"
        #define macro LEAF() = takes(0) returns(0) {
            0x01
        }
        
        #define macro BRANCH() = takes(0) returns(0) {
            LEAF()
        }
        
        #define macro MAIN() = takes(0) returns(0) {
            BRANCH()
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should succeed
    assert!(result.is_ok());
    let bytecode = result.unwrap();
    // Should produce valid bytecode with PUSH1 0x01
    assert!(bytecode.contains("6001"));
}

#[test]
fn test_constructor_circular_recursion() {
    // Circular recursion in CONSTRUCTOR macro
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            CONSTRUCTOR()
        }
        
        #define macro MAIN() = takes(0) returns(0) {
            0x00
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    // derive_storage_pointers should handle circular recursion in constructor gracefully
    contract.derive_storage_pointers(); // Should not panic

    // Constructor bytecode generation should also handle it
    let result = Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular macro invocation error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, CodegenErrorKind::CircularMacroInvocation("CONSTRUCTOR".to_string()));
}

#[test]
fn test_deep_recursion_limit() {
    // Test that we handle deep but non-circular recursion gracefully
    // This creates a chain of 10 unique macros
    let source = r#"
        #define macro M0() = takes(0) returns(0) { 0x00 }
        #define macro M1() = takes(0) returns(0) { M0() }
        #define macro M2() = takes(0) returns(0) { M1() }
        #define macro M3() = takes(0) returns(0) { M2() }
        #define macro M4() = takes(0) returns(0) { M3() }
        #define macro M5() = takes(0) returns(0) { M4() }
        #define macro M6() = takes(0) returns(0) { M5() }
        #define macro M7() = takes(0) returns(0) { M6() }
        #define macro M8() = takes(0) returns(0) { M7() }
        #define macro M9() = takes(0) returns(0) { M8() }
        
        #define macro MAIN() = takes(0) returns(0) {
            M9()
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // This should succeed as it's not circular, just deep
    assert!(result.is_ok());
    let bytecode = result.unwrap();
    // Should contain PUSH1 0x00
    assert!(bytecode.contains("5f"));
}
