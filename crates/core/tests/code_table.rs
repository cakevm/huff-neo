use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::Token;

#[test]
fn test_code_table_multiple_linex() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0x1234
            0xDEADBEEF
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 06 = 6 bytes table size, 61 = PUSH2, 0005 = 5 bytes table start
    assert_eq!(mbytes, "60 06 61 0005 1234 deadbeef".replace(" ", ""));
}

#[test]
fn test_code_table_with_func_sig() {
    let source: &str = r#"
        #define table CODE_TABLE {
            __FUNC_SIG("transfer(address,uint256)")
            0xDEADBEEF
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 08 = 8 bytes table size, 61 = PUSH2, 0005 = 5 bytes table start, a9059cbb = transfer(address,uint256)
    assert_eq!(mbytes, "60 08 61 0005 a9059cbb deadbeef".replace(" ", ""));
}

#[test]
fn test_code_table_constant() {
    let source: &str = r#"
        #define constant ADDR = 0x0101010101010101010101010101010101010101
        #define table CODE_TABLE() {
            [ADDR]
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 14 = 20 bytes table size, 61 = PUSH2, 0005 = 5 bytes table start
    assert_eq!(mbytes, "60 14 61 0005 0101010101010101010101010101010101010101".replace(" ", ""));
}

#[test]
fn test_code_table_leftpad() {
    let source: &str = r#"
        #define table CODE_TABLE() {
            __LEFTPAD(0x0123)
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 20 = 32 bytes table size, 61 = PUSH2, 0005 = 5 bytes table start
    assert_eq!(mbytes, "60 20 61 0005 0000000000000000000000000000000000000000000000000000000000000123".replace(" ", ""));
}

#[test]
fn test_code_table_constant_with_leading_zeros() {
    let source: &str = r#"
        #define constant ADDR = 0x00000001
        #define table CODE_TABLE() {
            [ADDR]
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 04 = 4 bytes table size, 61 = PUSH2, 0005 = 5 bytes table start
    assert_eq!(mbytes, "60 04 61 0005 00000001".replace(" ", ""));
}

#[test]
fn test_code_table_with_right_pad_func_sig() {
    let source: &str = r#"
        #define table CODE_TABLE {
            __RIGHTPAD(__FUNC_SIG("transfer(address,uint256)"))
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 20 = 32 bytes table size, 61 = PUSH2, 0005 = 5 bytes table start, a9059cbb = transfer(address,uint256)
    assert_eq!(mbytes, "60 20 61 0005 a9059cbb00000000000000000000000000000000000000000000000000000000".replace(" ", ""));
}

#[test]
fn test_code_table_all_features() {
    let source: &str = r#"
        #define constant ADDR = 0x0101010101010101010101010101010101010101
        #define table CODE_TABLE {
            __RIGHTPAD(__FUNC_SIG("transfer(address,uint256)"))
            0xDEADBEEF
            [ADDR]
            __LEFTPAD(0x123)
            __LEFTPAD([ADDR])
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
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

    // Have the Codegen create the constructor bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // 60 = PUSH1, 78 = 120 bytes table size (32 + 4 + 20), 61 = PUSH2, 0005 = 5 bytes table start, a9059cbb = transfer(address,uint256)
    assert_eq!(
        mbytes,
        "60 78 61 0005 a9059cbb00000000000000000000000000000000000000000000000000000000 deadbeef \
        0101010101010101010101010101010101010101 \
        0000000000000000000000000000000000000000000000000000000000000123\
        0000000000000000000000000101010101010101010101010101010101010101"
            .replace(" ", "")
    );
}
