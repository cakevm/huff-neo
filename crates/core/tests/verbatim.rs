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

#[test]
fn test_verbatim_with_nested_func_sig() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(__FUNC_SIG("withdraw(uint256)"))
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // keccak256("withdraw(uint256)")[0:4] = 0x2e1a7d4d
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "2e1a7d4d".to_string()),
        Err(e) => panic!("Failed to generate bytecode: {:?}", e),
    }
}

#[test]
fn test_verbatim_with_nested_event_hash() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(__EVENT_HASH("Transfer(address,address,uint256)"))
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // keccak256("Transfer(address,address,uint256)")
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef".to_string()),
        Err(e) => panic!("Failed to generate bytecode: {:?}", e),
    }
}

#[test]
fn test_verbatim_with_nested_bytes() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(__BYTES("hello"))
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // "hello" in UTF-8 hex = 68656c6c6f
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "68656c6c6f".to_string()),
        Err(e) => panic!("Failed to generate bytecode: {:?}", e),
    }
}

#[test]
fn test_verbatim_with_nested_error() {
    let source = r#"
    #define error CustomError()

    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(__ERROR(CustomError))
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // __ERROR with error definition puts selector at start, right-padded to 32 bytes
    // keccak256("CustomError()")[0:4] = 0x09caebf3
    // Right-padded to 32 bytes: 09caebf3000...000
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "09caebf300000000000000000000000000000000000000000000000000000000".to_string()),
        Err(e) => panic!("Failed to generate bytecode: {:?}", e),
    }
}

#[test]
fn test_verbatim_with_nested_rightpad() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(__RIGHTPAD(0x1234))
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // __RIGHTPAD(0x1234) = 0x1234 followed by 30 zero bytes
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "1234000000000000000000000000000000000000000000000000000000000000".to_string()),
        Err(e) => panic!("Failed to generate bytecode: {:?}", e),
    }
}

#[test]
fn test_verbatim_with_nested_leftpad() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        __VERBATIM(__LEFTPAD(0x1234))
    }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // __LEFTPAD(0x1234) = 30 zero bytes followed by 0x1234
    match Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false) {
        Ok(mb) => assert_eq!(mb, "0000000000000000000000000000000000000000000000000000000000001234".to_string()),
        Err(e) => panic!("Failed to generate bytecode: {:?}", e),
    }
}
