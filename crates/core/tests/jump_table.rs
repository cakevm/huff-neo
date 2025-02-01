use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::Token;

#[test]
fn test_tablestart_builtin() {
    let source: &str = r#"
        #define jumptable__packed PACKED_JUMPTABLE {
            lab_0 lab_1 lab_2 lab_3
        }

        #define jumptable STANDARD_JUMPTABLE {
            lab_0 // 0 00000
            lab_1 // 1 00001
            lab_2 // 2 00010
            lab_3 // 3 00011
        }

        #define macro BUILTIN_TEST() = takes(0) returns(2) {
            __tablestart(PACKED_JUMPTABLE)
            __tablestart(STANDARD_JUMPTABLE)
        }

        #define macro CONSTRUCTOR() = takes(0) returns (0) {
            BUILTIN_TEST()

            lab_0:
                0x00
                0x00
                return
            lab_1:
                0x00
                0x00
                return
            lab_2:
                0x00
                0x00
                return
            lab_3:
                0x00
                0x00
                return
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
    let (cbytes, custom_bootstrap) = Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None).unwrap();
    assert_eq!(cbytes, String::from("61001661001e5b5f5ff35b5f5ff35b5f5ff35b5f5ff30006000a000e00120000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000012"));
    assert!(custom_bootstrap);
}

#[test]
fn test_jump_table_exhaustive_usage() {
    let source: &str = r#"
        #define jumptable STANDARD_JUMPTABLE {
            lab_0 // 0 00000
            lab_1 // 1 00001
            lab_2 // 2 00010
            lab_3 // 3 00011
        }

        // Copies the standard table into memory with codecopy
        #define macro INIT_JUMP_TABLE() = takes(0) returns(1) {
            __tablesize(STANDARD_JUMPTABLE) __tablestart(STANDARD_JUMPTABLE) 0x00 codecopy
        }

        #define macro COMPUTE() = takes (0) returns (1) {
            0x20 dup8 sub mload 0x02ffe0 and
            dup1 0x20 add

            lab_0:
                0x20 0x20 add
            lab_1:
                0x20 0x20 add
            lab_2:
                0x20 0x20 add
            lab_3:
                0x20 0x20 add
        }

        #define macro MAIN() = takes(0) returns (0) {
            INIT_JUMP_TABLE()

            0x00 calldataload 0xE0 shr
            dup1 0xa9059cbb eq compute jumpi

            compute:
                COMPUTE()
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
    assert_eq!(mbytes, String::from("608061003e5f395f3560e01c8063a9059cbb14610017575b60208703516202ffe016806020015b60206020015b60206020015b60206020015b60206020010000000000000000000000000000000000000000000000000000000000000026000000000000000000000000000000000000000000000000000000000000002c00000000000000000000000000000000000000000000000000000000000000320000000000000000000000000000000000000000000000000000000000000038"));
}

#[test]
fn test_jump_table_packed_exhaustive_usage() {
    let source: &str = r#"
        #define jumptable__packed PACKED_JUMPTABLE {
            lab_0 lab_1 lab_2 lab_3
        }

        // Copies the standard table into memory with codecopy
        #define macro INIT_JUMP_TABLE() = takes(0) returns(1) {
            __tablesize(PACKED_JUMPTABLE) __tablestart(PACKED_JUMPTABLE) 0x00 codecopy
        }

        #define macro COMPUTE() = takes (0) returns (1) {
            0x20 dup8 sub mload 0x02ffe0 and
            dup1 0x20 add

            lab_0:
                0x20 0x20 add
            lab_1:
                0x20 0x20 add
            lab_2:
                0x20 0x20 add
            lab_3:
                0x20 0x20 add
        }

        #define macro MAIN() = takes(0) returns (0) {
            INIT_JUMP_TABLE()

            0x00 calldataload 0xE0 shr
            dup1 0xa9059cbb eq compute jumpi

            compute:
                COMPUTE()
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

    // Have the Codegen create the main macro bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();
    assert_eq!(mbytes, String::from("600861003e5f395f3560e01c8063a9059cbb14610017575b60208703516202ffe016806020015b60206020015b60206020015b60206020015b60206020010026002c00320038"));
}

#[test]
fn test_label_clashing() {
    let source: &str = r#"
        #define jumptable__packed PACKED_JUMPTABLE {
            lab_0 lab_1 lab_2 lab_3
        }

        #define jumptable STANDARD_JUMPTABLE {
            lab_0 // 0 00000
            lab_1 // 1 00001
            lab_2 // 2 00010
            lab_3 // 3 00011
        }

        #define macro INIT_JUMP_TABLES() = takes(0) returns(1) {
            __tablesize(PACKED_JUMPTABLE) __tablestart(PACKED_JUMPTABLE) 0x00 codecopy
            __tablesize(STANDARD_JUMPTABLE) __tablestart(STANDARD_JUMPTABLE) 0x00 codecopy
        }

        #define macro COMPUTE() = takes (0) returns (1) {
            0x20 dup8 sub mload 0x02ffe0 and
            dup1 0x20 add

            lab_0:
                0x20 0x20 add
            lab_1:
                0x20 0x20 add
            lab_2:
                0x20 0x20 add
            lab_3:
                0x20 0x20 add
        }

        #define macro MAIN() = takes(0) returns (0) {
            INIT_JUMP_TABLES()

            0x00 calldataload 0xE0 shr
            dup1 0xa9059cbb eq compute jumpi

            compute:
                COMPUTE()
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

    // Have the Codegen create the main macro bytecode
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();
    assert_eq!(mbytes, String::from("60086100455f39608061004d5f395f3560e01c8063a9059cbb1461001e575b60208703516202ffe016806020015b60206020015b60206020015b60206020015b6020602001002d00330039003f000000000000000000000000000000000000000000000000000000000000002d00000000000000000000000000000000000000000000000000000000000000330000000000000000000000000000000000000000000000000000000000000039000000000000000000000000000000000000000000000000000000000000003f"));
}
