use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::Token;

#[test]
fn test_jump_table_in_nested_macro() {
    let source: &str = r#"
        #define jumptable JUMP_TABLE {
            lab_0
        }

        #define macro INNER() = takes(0) returns(1) {
            __tablestart(JUMP_TABLE)
        }

        #define macro OUTER() = takes(0) returns(1) {
            INNER()
        }

        #define macro MAIN() = takes(0) returns (0) {
            OUTER()

            lab_0:
                0x00
                0x00
                return
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let cg = Codegen::new();
    assert!(cg.artifact.is_none());

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // PUSH2 0x0007 JUMPDEST PUSH0 PUSH0 RETURN -> 32 bytes, jump dest 3
    // table start is 0x0007
    assert_eq!(mbytes, "61 0007 5b 5f 5f f3 0000000000000000000000000000000000000000000000000000000000000003".replace(" ", ""));
}

#[test]
fn test_tablesize_in_nested_macro() {
    let source: &str = r#"
        #define jumptable JUMP_TABLE {
            lab_0 lab_1
        }

        #define macro INNER() = takes(0) returns(2) {
            __tablesize(JUMP_TABLE)
        }

        #define macro OUTER() = takes(0) returns(2) {
            INNER()
        }

        #define macro MAIN() = takes(0) returns (0) {
            OUTER()

            lab_0:
                0x01
                return
            lab_1:
                0x02
                return
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let cg = Codegen::new();
    assert!(cg.artifact.is_none());

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // PUSH1 0x40 JUMPDEST PUSH1 0x01 RETURN JUMPDEST PUSH1 0x02 RETURN
    // table size is 0x40 = 64 bytes
    assert_eq!(
        mbytes,
        "60 40 5b 60 01 f3 5b 60 02 f3 0000000000000000000000000000000000000000000000000000000000000002 0000000000000000000000000000000000000000000000000000000000000006".replace(" ", ""));
}

#[test]
fn test_jump_table_nested_args() {
    let source: &str = r#"
        #define jumptable JUMP_TABLE {
            lab_0
        }

        #define macro NESTED() = {
            __tablestart(JUMP_TABLE)
        }

        #define macro MACRO(nested) = {
            <nested>
        }

        #define macro MAIN() = takes(0) returns (0) {
            MACRO(NESTED())

            lab_0:
                0x01
                return
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let cg = Codegen::new();
    assert!(cg.artifact.is_none());

    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();

    // PUSH2 0x0007 JUMPDEST PUSH1 0x01 RETURN -> 32 bytes, jump dest 3
    // table start is 0x0007
    assert_eq!(mbytes, "61 0007 5b 60 01 f3 0000000000000000000000000000000000000000000000000000000000000003".replace(" ", ""));
}
