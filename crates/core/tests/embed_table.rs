use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::error::CodegenErrorKind;
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::Token;

#[test]
fn test_embed_table_basic() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0x1234
            0xDEADBEEF
        }

        #define macro MAIN() = takes(0) returns (0) {
            __EMBED_TABLE(CODE_TABLE)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Table should be embedded inline, not at the end
    // Expected: just the table bytes directly
    assert_eq!(mbytes, "1234deadbeef");
}

#[test]
fn test_embed_table_with_tablestart() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0x1234
            0xDEADBEEF
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablestart(CODE_TABLE)  // Should return offset 0x0003 (after PUSH2)
            __EMBED_TABLE(CODE_TABLE)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Note: __tablestart is called before __EMBED_TABLE, so it uses a placeholder
    // The placeholder is PUSH2 by default (61 0003 where 0003 is filled)
    // 61 = PUSH2, 0003 = offset 3 (where table starts after PUSH2), then the table bytes
    assert_eq!(mbytes, "610003 1234deadbeef".replace(" ", ""));
}

#[test]
fn test_embed_table_not_at_end() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0xABCD
        }

        #define macro MAIN() = takes(0) returns (0) {
            __EMBED_TABLE(CODE_TABLE)
            0x42            // PUSH1 0x42
            0x01            // PUSH1 0x01
            mstore          // MSTORE
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Table embedded inline, followed by PUSH1, PUSH1, MSTORE, STOP
    // abcd = table (2 bytes)
    // 6042 = PUSH1 0x42 (2 bytes)
    // 6001 = PUSH1 0x01 (2 bytes)
    // 52 = MSTORE (1 byte)
    // 00 = STOP (1 byte)
    // Total: 8 bytes (no table duplication at end)
    assert_eq!(mbytes, "abcd604260015200");
}

#[test]
fn test_embed_table_with_func_sig() {
    let source: &str = r#"
        #define table CODE_TABLE {
            __FUNC_SIG("transfer(address,uint256)")
            0xDEADBEEF
        }

        #define macro MAIN() = takes(0) returns (0) {
            __EMBED_TABLE(CODE_TABLE)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // a9059cbb = transfer(address,uint256) selector
    assert_eq!(mbytes, "a9059cbb deadbeef".replace(" ", ""));
}

#[test]
fn test_embed_table_with_operations_before_and_after() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0x1234
        }

        #define macro MAIN() = takes(0) returns (0) {
            0x42            // PUSH1 0x42
            __EMBED_TABLE(CODE_TABLE)
            0x01            // PUSH1 0x01
            mstore          // MSTORE
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // 6042 = PUSH1 0x42 (2 bytes)
    // 1234 = table (2 bytes)
    // 6001 = PUSH1 0x01 (2 bytes)
    // 52 = MSTORE (1 byte)
    // Total: 7 bytes
    assert_eq!(mbytes, "6042 1234 6001 52".replace(" ", ""));
}

#[test]
fn test_embed_table_with_tablestart_and_surrounding_ops() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0xABCD
        }

        #define macro MAIN() = takes(0) returns (0) {
            0x42            // PUSH1 0x42 (2 bytes)
            __tablestart(CODE_TABLE)  // Should return 0x0005 (2 + 3 for PUSH2)
            __EMBED_TABLE(CODE_TABLE)
            0x01            // PUSH1 0x01
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // 6042 = PUSH1 0x42 (2 bytes)
    // 610005 = PUSH2 0x0005 (3 bytes) - offset where table starts
    // abcd = table (2 bytes) - at offset 0x0005
    // 6001 = PUSH1 0x01 (2 bytes)
    // 00 = STOP (1 byte)
    // Total: 10 bytes
    assert_eq!(mbytes, "6042 610005 abcd 6001 00".replace(" ", ""));
}

#[test]
fn test_multiple_embedded_tables() {
    let source: &str = r#"
        #define table TABLE_A {
            0x1111
        }

        #define table TABLE_B {
            0x2222
        }

        #define macro MAIN() = takes(0) returns (0) {
            __tablestart(TABLE_A)   // Should be 0x0003
            __tablestart(TABLE_B)   // Should be 0x0008 (3 + 2 table_a + 3)
            __EMBED_TABLE(TABLE_A)  // at 0x0006
            __EMBED_TABLE(TABLE_B)  // at 0x0008
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // 610006 = PUSH2 0x0006 (3 bytes) - offset where TABLE_A starts
    // 610008 = PUSH2 0x0008 (3 bytes) - offset where TABLE_B starts
    // 1111 = TABLE_A (2 bytes) - at offset 0x0006
    // 2222 = TABLE_B (2 bytes) - at offset 0x0008
    // 00 = STOP (1 byte)
    // Total: 11 bytes
    assert_eq!(mbytes, "610006 610008 1111 2222 00".replace(" ", ""));
}

#[test]
fn test_embed_table_duplicate_embedding_fails() {
    let source: &str = r#"
        #define table CODE_TABLE {
            0xABCD
        }

        #define macro MAIN() = takes(0) returns (0) {
            __EMBED_TABLE(CODE_TABLE)
            __EMBED_TABLE(CODE_TABLE)  // Second embedding should fail
            stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    // Should return an error because CODE_TABLE is embedded twice
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, CodegenErrorKind::DuplicateTableEmbedding("CODE_TABLE".to_string()));
}
