use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::Token;
use std::sync::Arc;

mod common;
use common::compile_to_deployment;

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
    let (cbytes, custom_bootstrap) = Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(
        cbytes,
        String::from(
            "61001661001e5b5f5ff35b5f5ff35b5f5ff35b5f5ff30006000a000e00120000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000012"
        )
    );
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
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(
        mbytes,
        String::from(
            "608061003e5f395f3560e01c8063a9059cbb14610017575b60208703516202ffe016806020015b60206020015b60206020015b60206020015b60206020010000000000000000000000000000000000000000000000000000000000000026000000000000000000000000000000000000000000000000000000000000002c00000000000000000000000000000000000000000000000000000000000000320000000000000000000000000000000000000000000000000000000000000038"
        )
    );
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
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(
        mbytes,
        String::from(
            "600861003e5f395f3560e01c8063a9059cbb14610017575b60208703516202ffe016806020015b60206020015b60206020015b60206020015b60206020010026002c00320038"
        )
    );
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
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(
        mbytes,
        String::from(
            "60086100455f39608061004d5f395f3560e01c8063a9059cbb1461001e575b60208703516202ffe016806020015b60206020015b60206020015b60206020015b6020602001002d00330039003f000000000000000000000000000000000000000000000000000000000000002d00000000000000000000000000000000000000000000000000000000000000330000000000000000000000000000000000000000000000000000000000000039000000000000000000000000000000000000000000000000000000000000003f"
        )
    );
}

/// Invariant: a code table referenced from the CONSTRUCTOR must not be inserted between the
/// constructor body and the auto-generated bootstrap. If it is, execution falls into the table
/// bytes (INVALID opcode) before the bootstrap can run and deployment fails. The table belongs
/// in the deployment tail, past `MAIN`.
#[test]
fn test_constructor_table_does_not_break_auto_bootstrap() {
    let source = r#"
        #define macro CONSTRUCTOR() = { __tablestart(TABLE) pop }
        #define macro MAIN() = {}
        #define table TABLE { 0xfefe }
    "#;
    let bytecode = compile_to_deployment(source);

    // Expected layout: [ctor_body (4)][bootstrap (9)][main (0)][table (2)]
    //   61 000d                  PUSH2 0x000d   (tablestart resolves to byte 13)
    //   50                       POP
    //   60 00 80 60 0d 3d 39 3d f3   bootstrap (9 bytes)
    //   fe fe                    table data at byte 13
    assert_eq!(bytecode, "61000d50600080600d3d393df3fefe");
}

/// CONSTRUCTOR uses `__tablestart` with a non-trivial MAIN. The constructor-only table sits past
/// main in the deployment tail; the deployed runtime equals exactly MAIN's bytecode (the table is
/// not part of runtime).
#[test]
fn test_constructor_table_with_nontrivial_main() {
    let source = r#"
        #define macro CONSTRUCTOR() = { __tablestart(TABLE) pop }
        #define macro MAIN() = { 0x01 0x02 add }
        #define table TABLE { 0xdead }
    "#;
    let evm = &EVMVersion::default();
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, false).unwrap();
    let main_len = main.bytecode.len() / 2;
    let (constructor, updated_contract) = Codegen::generate_constructor_macro_bytecode(evm, &contract, None, false).unwrap();

    let mut cg = Codegen::new();
    let artifact =
        cg.assemble_artifact(Arc::new(FileSource::default()), &updated_contract, vec![], main.clone(), Some(constructor), false).unwrap();

    // Runtime is exactly MAIN bytecode — table is not deployed.
    assert_eq!(artifact.runtime, main.bytecode.to_lowercase());

    // Tablestart in constructor points at ctor_body_len + bootstrap_size + main_len.
    // ctor_body_len = 4, bootstrap_size = 9 (PUSH1 main_len, PUSH1 runtime_offset both fit), main_len = 5.
    let expected_table_offset = 4 + 9 + main_len;
    assert_eq!(expected_table_offset, 18);
    let tablestart_value = u16::from_str_radix(&artifact.bytecode[2..6], 16).unwrap() as usize;
    assert_eq!(tablestart_value, expected_table_offset);

    // Table bytes "dead" are the last 4 hex chars of the deployment bytecode.
    assert!(artifact.bytecode.ends_with("dead"));
}

/// When the combined `ctor_body + bootstrap` crosses 0xff, the runtime offset push in the
/// bootstrap must grow from PUSH1 to PUSH2 and `bootstrap_code_size` must account for the extra
/// byte. Exercise that branch by padding the constructor body to push the runtime offset above 255.
#[test]
fn test_constructor_table_offset_exceeds_one_byte() {
    // 256 bytes of constructor body (256 JUMPDESTs) ensures bootstrap_size + ctor_body_len > 0xff
    // so the runtime offset has to be PUSH2.
    let mut body = String::new();
    for _ in 0..256 {
        body.push_str("jumpdest "); // 1 byte each, safe filler.
    }
    let source = format!(
        r#"
        #define macro CONSTRUCTOR() = {{
            __tablestart(TABLE) pop
            {body}
        }}
        #define macro MAIN() = {{}}
        #define table TABLE {{ 0xfefe }}
    "#
    );
    let bytecode = compile_to_deployment(&source);

    // ctor_body_len = 4 (tablestart push2 + pop + nothing) + 256 (filler) = 260.
    // Runtime offset = bootstrap_size + ctor_body_len = 10 + 260 = 270 = 0x010e (needs PUSH2).
    // bootstrap layout: PUSH1 size, DUP1, PUSH2 offset, RETURNDATASIZE, CODECOPY, RETURNDATASIZE, RETURN = 10 bytes.
    // Bootstrap starts at offset 260; the PUSH2 immediate at offset 260+3 = 263 holds 0x010e.
    // Verify the table sits at offset 270 (after bootstrap end).
    let table_offset_in_bytecode = 270;
    let expected_table_hex = format!("{:02x}", table_offset_in_bytecode);
    // tablestart PUSH2 immediate is at bytes 1-2 (chars 2-6) of the bytecode.
    let tablestart = u16::from_str_radix(&bytecode[2..6], 16).unwrap();
    assert_eq!(tablestart as usize, table_offset_in_bytecode, "tablestart should resolve past bootstrap");
    assert!(bytecode.ends_with("fefe"), "table bytes appear at the deployment tail");
    let _ = expected_table_hex; // kept for clarity of intent
}

/// When the same table is referenced from both CONSTRUCTOR and MAIN, it is emitted exactly once
/// (in the runtime section). The constructor's `__tablestart` resolves into that shared copy at
/// `bootstrap_size + ctor_body_len + main_body_len + offset_within_runtime`.
#[test]
fn test_table_shared_between_constructor_and_main_emitted_once() {
    let source = r#"
        #define macro CONSTRUCTOR() = { __tablestart(SHARED) pop }
        #define macro MAIN() = { __tablestart(SHARED) pop }
        #define table SHARED { 0xcafe }
    "#;
    let evm = &EVMVersion::default();
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, false).unwrap();
    let main_body_len = main.body_len();
    let shared_offset_in_main = *main.table_offsets.get("SHARED").expect("SHARED table emitted in main");
    let (constructor, updated_contract) = Codegen::generate_constructor_macro_bytecode(evm, &contract, None, false).unwrap();
    let ctor_body_len: usize = constructor.bytecode_res.bytes.iter().map(|s| s.bytes.len()).sum();

    let mut cg = Codegen::new();
    let artifact =
        cg.assemble_artifact(Arc::new(FileSource::default()), &updated_contract, vec![], main, Some(constructor), false).unwrap();

    // The table bytes ("cafe") appear exactly once in the deployment bytecode.
    let occurrences = artifact.bytecode.matches("cafe").count();
    assert_eq!(occurrences, 1, "shared table must be emitted only once");

    // Constructor's __tablestart resolves into the runtime-resident copy.
    // bootstrap_size: contract_length = main_body_len + 2 (table); both <256 → bootstrap_size = 9.
    let bootstrap_size = 9;
    let ctor_tablestart_expected = bootstrap_size + ctor_body_len + shared_offset_in_main;
    let ctor_tablestart = u16::from_str_radix(&artifact.bytecode[2..6], 16).unwrap() as usize;
    assert_eq!(ctor_tablestart, ctor_tablestart_expected);

    // Main's __tablestart resolves to the table offset within the runtime. The runtime starts at
    // deployment offset ctor_body_len + bootstrap_size in the deployment bytecode. The main
    // tablestart PUSH2 immediate lives at the start of the main body.
    let main_start_in_deployment = ctor_body_len + bootstrap_size;
    let main_tablestart_chars = &artifact.bytecode[(main_start_in_deployment + 1) * 2..(main_start_in_deployment + 3) * 2];
    let main_tablestart = u16::from_str_radix(main_tablestart_chars, 16).unwrap() as usize;
    // shared_offset_in_main already equals body_len + offset-into-tables, since gen_table_bytecode
    // stores the absolute (within-runtime) offset directly.
    assert_eq!(main_tablestart, shared_offset_in_main);
    assert!(main_tablestart >= main_body_len, "table sits after the runtime body");
}

/// CONSTRUCTOR-only and MAIN-only tables coexist: the ctor-only table is appended past main in the
/// deployment tail and never enters runtime; the main-only table sits in runtime as usual.
#[test]
fn test_constructor_only_and_main_only_tables_coexist() {
    let source = r#"
        #define macro CONSTRUCTOR() = { __tablestart(CTOR_ONLY) pop }
        #define macro MAIN() = { __tablestart(MAIN_ONLY) pop }
        #define table CTOR_ONLY { 0xaaaa }
        #define table MAIN_ONLY { 0xbbbb }
    "#;
    let evm = &EVMVersion::default();
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, false).unwrap();
    let (constructor, updated_contract) = Codegen::generate_constructor_macro_bytecode(evm, &contract, None, false).unwrap();

    let mut cg = Codegen::new();
    let artifact =
        cg.assemble_artifact(Arc::new(FileSource::default()), &updated_contract, vec![], main.clone(), Some(constructor), false).unwrap();

    // CTOR_ONLY table only appears in deployment, never in runtime.
    assert!(artifact.bytecode.contains("aaaa"));
    assert!(!artifact.runtime.contains("aaaa"));

    // MAIN_ONLY table appears in both deployment and runtime.
    assert!(artifact.runtime.contains("bbbb"));
    assert_eq!(artifact.bytecode.matches("bbbb").count(), 1);

    // CTOR_ONLY is at the very end of deployment (past constructor_args, which are empty here).
    assert!(artifact.bytecode.ends_with("aaaa"));
}

/// `relax_jumps` shrinks JUMP/JUMPI placeholders but must not touch `__tablestart` placeholders
/// (which are always PUSH2). Verify the layout invariants still hold when relaxation is enabled.
#[test]
fn test_constructor_table_with_relax_jumps() {
    let source = r#"
        #define macro CONSTRUCTOR() = {
            __tablestart(TABLE) pop
            target jump
            target:
        }
        #define macro MAIN() = {}
        #define table TABLE { 0xfefe }
    "#;
    let evm = &EVMVersion::default();
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // relax_jumps = true.
    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, true).unwrap();
    let (constructor, updated_contract) = Codegen::generate_constructor_macro_bytecode(evm, &contract, None, true).unwrap();

    let mut cg = Codegen::new();
    let artifact =
        cg.assemble_artifact(Arc::new(FileSource::default()), &updated_contract, vec![], main, Some(constructor), false).unwrap();

    // Table bytes still sit at the very end of deployment (past the bootstrap).
    assert!(artifact.bytecode.ends_with("fefe"));
    // tablestart placeholder is still PUSH2 (always), so the bytecode starts with `61` (PUSH2 opcode).
    assert!(artifact.bytecode.starts_with("61"));
}
