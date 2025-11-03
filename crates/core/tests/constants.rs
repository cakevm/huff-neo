use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::error::CodegenErrorKind;
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::Token;

#[test]
fn test_builtin_rightpad_bytes() {
    let source: &str = r#"
        #define constant FUNC = __FUNC_SIG("transfer(address,uint256)")

        #define macro MAIN() = takes (0) returns (0) {
            [FUNC]
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();

    // Derive storage pointers
    contract.derive_storage_pointers();

    // Instantiate Codegen
    let cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    // Have Codegen create the runtime bytecode
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();
    // PUSH4 = 0x63, `transfer(address,uint256) signature = 0xa9059cbb
    assert_eq!(&r_bytes, "63a9059cbb");
}

#[test]
fn test_direct_circular_constant_dependency() {
    // Direct circular dependency: A = B, B = A
    let source = r#"
        #define constant A = [B]
        #define constant B = [A]

        #define macro MAIN() = takes(0) returns(0) {
            [A]
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular constant dependency error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, CodegenErrorKind::CircularConstantDependency(_)));
}

#[test]
fn test_indirect_circular_constant_dependency() {
    // Indirect circular dependency: A = B, B = C, C = A
    let source = r#"
        #define constant A = [B]
        #define constant B = [C]
        #define constant C = [A]

        #define macro MAIN() = takes(0) returns(0) {
            [A]
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular constant dependency error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, CodegenErrorKind::CircularConstantDependency(_)));
}

#[test]
fn test_self_referencing_constant() {
    // Self-referencing constant: A = A + 1
    let source = r#"
        #define constant A = [A] + 1

        #define macro MAIN() = takes(0) returns(0) {
            [A]
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();

    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular constant dependency error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, CodegenErrorKind::CircularConstantDependency("A".to_string()));
}

#[test]
fn test_valid_constant_references() {
    // Valid case: constants that reference other constants without cycles
    let source = r#"
        #define constant A = 0x01
        #define constant B = [A] + 0x02
        #define constant C = [B] * 0x03

        #define macro MAIN() = takes(0) returns(0) {
            [C]
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
    // Should produce bytecode with the calculated value of C = (1 + 2) * 3 = 9
    assert!(bytecode.contains("6009")); // PUSH1 0x09
}

#[test]
fn test_circular_dependency_in_constructor() {
    // Circular constant dependency used in CONSTRUCTOR
    let source = r#"
        #define constant A = [B]
        #define constant B = [A]

        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            [A]
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

    contract.derive_storage_pointers();

    let result = Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with circular constant dependency error
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, CodegenErrorKind::CircularConstantDependency(_)));
}
