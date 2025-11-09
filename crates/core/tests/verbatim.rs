use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_verbatim() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(0x1234567890abcdef)
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Get main bytecode with verbatim
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "1234567890abcdef".to_string()),
        Err(_) => panic!("moose"),
    }
}

#[test]
fn test_verbatim_invalid_hex() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM("ggggggg")
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Expect failure to generate bytecode with verbatim
    assert!(Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).is_err());
}

#[test]
fn test_verbatim_with_constant() {
    let source = r#"
    #define constant MY_BYTECODE = 0xaabbccdd

    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM([MY_BYTECODE])
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Get main bytecode with verbatim constant
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "aabbccdd".to_string()),
        Err(_) => panic!("moose"),
    }
}

#[test]
fn test_verbatim_with_builtin_constant() {
    let source = r#"
    #define constant MY_SIG = __FUNC_SIG("transfer(address,uint256)")

    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM([MY_SIG])
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Get main bytecode with verbatim constant
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "a9059cbb".to_string()),
        Err(_) => panic!("moose"),
    }
}
