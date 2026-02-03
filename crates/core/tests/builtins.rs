use crate::common::{assert_compile_error, compile_to_bytecode};
use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;
use std::sync::Arc;

#[test]
fn test_codesize_builtin() {
    let source: &str = r#"
        #define constant OWNER_POINTER = FREE_STORAGE_POINTER()

        #define macro OWNABLE() = takes (0) returns (0) {
            caller [OWNER_POINTER] sstore
        }

        #define macro BUILTIN_TEST() = takes(0) returns(1) {
            __codesize(OWNABLE)
        }

        #define macro CONSTRUCTOR() = takes(0) returns (0) {
            BUILTIN_TEST()
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
    assert_eq!(cbytes, String::from("6003"));
    assert!(!custom_bootstrap);
}

#[test]
fn test_dyn_constructor_arg_builtin() {
    let source: &str = r#"
        #define macro MAIN() = {
            // Store first dynamic constructor argument in memory @ 0x20
            __CODECOPY_DYN_ARG(0x00, 0x20)
        }

        #define macro CONSTRUCTOR() = {}
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
    let mut cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    let evm_version = &EVMVersion::default();

    // Have the Codegen create the constructor bytecode
    let (constructor_code, has_custom_bootstrap) = Codegen::generate_constructor_bytecode(evm_version, &contract, None, false).unwrap();
    let main_code = Codegen::generate_main_bytecode(evm_version, &contract, None, false).unwrap();

    let args = Codegen::encode_constructor_args(vec![String::from("testing")]);
    let final_bytecode = cg.churn(
        Arc::new(FileSource::default()),
        args,
        main_code.as_str(),
        constructor_code.as_str(),
        has_custom_bootstrap,
        None,
        None,
        false,
    );

    assert_eq!(
        final_bytecode.unwrap().bytecode,
        String::from(
            "60118060093d393df3610007610020526100076100116100403974657374696e6700000000000000000000000000000000000000000000000000"
        )
    );
}

#[test]
fn test_tablesize_builtin() {
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

        #define table CODE_TABLE {
            0xDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEF
        }

        #define macro TEST_INIT_JUMP_TABLE() = takes(0) returns(1) {
            __tablesize(STANDARD_JUMPTABLE) __tablestart(STANDARD_JUMPTABLE) 0x00 codecopy
        }

        #define macro BUILTIN_TEST() = takes(0) returns(1) {
            __tablesize(PACKED_JUMPTABLE)
        }

        #define macro BUILTIN_TEST_2() = takes(0) returns(1) {
            __tablesize(CODE_TABLE)
        }

        #define macro MAIN() = takes(0) returns (0) {
            BUILTIN_TEST()
            TEST_INIT_JUMP_TABLE()
            BUILTIN_TEST_2()

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
    let mbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(
        mbytes,
        String::from(
            "600860806100235f3960205b5f5ff35b5f5ff35b5f5ff35b5f5ff3000b000f00130017000000000000000000000000000000000000000000000000000000000000000b000000000000000000000000000000000000000000000000000000000000000f00000000000000000000000000000000000000000000000000000000000000130000000000000000000000000000000000000000000000000000000000000017deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
        )
    );
}

#[test]
fn test_func_sig_builtin() {
    let source: &str = r#"
        #define function transfer(address,uint256) nonpayable returns ()

        #define macro TRANSFER() = takes (0) returns (0) {
            // ...
        }

        #define macro MAIN() = takes(0) returns (0) {
            // Identify which function is being called.
            0x00 calldataload 0xE0 shr
            dup1 __FUNC_SIG("transfer(address,uint256)") eq transfer jumpi
            dup1 __FUNC_SIG('transfer(address,uint256)') eq transfer jumpi
            dup1 __FUNC_SIG(transfer) eq transfer jumpi

            transfer:
                TRANSFER()
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
    let cbytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    // `transfer(address,uint256) signature = 0xa9059cbb
    assert_eq!(&cbytes[14..22], "a9059cbb");
    assert_eq!(&cbytes[36..44], "a9059cbb");
    assert_eq!(&cbytes[58..66], "a9059cbb");
    assert_eq!(cbytes, String::from("5f3560e01c8063a9059cbb14610026578063a9059cbb14610026578063a9059cbb14610026575b"));
}

#[test]
fn test_event_hash_builtin() {
    let source: &str = r#"
        #define event transfer(address,address,uint256)

        #define macro MAIN() = takes(0) returns (0) {
            __EVENT_HASH("transfer(address,address,uint256)")
            __EVENT_HASH('transfer(address,address,uint256)')
            __EVENT_HASH(transfer)
            0x00 sstore
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let evm_version = EVMVersion::default();

    // Parse the AST
    let mut contract = parser.parse().unwrap();

    // Derive storage pointers
    contract.derive_storage_pointers();

    // Instantiate Codegen
    let cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    // Have the Codegen create the constructor bytecode
    let cbytes = Codegen::generate_main_bytecode(&evm_version, &contract, None, false).unwrap();
    // `transfer(address,address,uint256) signature =
    // 0xbeabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a8
    assert_eq!(&cbytes[2..66], "beabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a8");
    assert_eq!(&cbytes[68..132], "beabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a8");
    assert_eq!(&cbytes[134..198], "beabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a8");
    assert_eq!(
        cbytes,
        String::from(
            "7fbeabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a87fbeabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a87fbeabacc8ffedac16e9a60acdb2ca743d80c2ebb44977a93fa8e483c74d2b35a85f55"
        )
    );
}

#[test]
fn test_error_selector_builtin() {
    let source: &str = r#"
        // Define our custom error
        #define error PanicError(uint256 panicCode)
        #define error Error(string)

        #define macro PANIC() = takes (1) returns (0) {
            // Input stack:          [panic_code]
            __ERROR(PanicError)   // [panic_error_selector, panic_code]
            0x00 mstore           // [panic_code]
            0x04 mstore           // []
            0x24 0x00 revert
        }

        #define macro REQUIRE() = takes (3) returns (0) {
            // Input stack:          [condition, message_length, message]
            continue jumpi        // [message]

            __ERROR("Error(string)")        // [error_selector, message_length, message]
            0x00 mstore           // [message_length, message]
            0x20 0x04 mstore      // [message_length, message]
            0x24 mstore           // [message]
            0x44 mstore           // []

            0x64 0x00 revert

            continue:
                pop               // []
        }

        #define macro MAIN() = takes (0) returns (0) {
            // dummy macro invocations so they're included in the runtime bytecode
            PANIC()
            REQUIRE()
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
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(&r_bytes[2..66], "be20788c00000000000000000000000000000000000000000000000000000000");
    assert_eq!(&r_bytes[94..102], "08c379a0");
    assert_eq!(
        r_bytes,
        String::from(
            "7fbe20788c000000000000000000000000000000000000000000000000000000005f5260045260245ffd610044576308c379a05f52602060045260245260445260645ffd5b50"
        )
    );
}

#[test]
fn test_rightpad_builtin() {
    let source: &str = r#"
        #define macro MAIN() = takes (0) returns (0) {
            __RIGHTPAD(0xa57b)
            __RIGHTPAD(0x48656c6c6f2c20576f726c6421)
            __RIGHTPAD(0x6d6f6f7365)
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
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    assert_eq!(&r_bytes[2..66], "a57b000000000000000000000000000000000000000000000000000000000000");
    assert_eq!(&r_bytes[68..132], "48656c6c6f2c20576f726c642100000000000000000000000000000000000000");
    assert_eq!(&r_bytes[134..198], "6d6f6f7365000000000000000000000000000000000000000000000000000000");
    assert_eq!(r_bytes.len(), (32 * 3 + 3) * 2);
    assert_eq!(
        r_bytes,
        String::from(
            "7fa57b0000000000000000000000000000000000000000000000000000000000007f48656c6c6f2c20576f726c6421000000000000000000000000000000000000007f6d6f6f7365000000000000000000000000000000000000000000000000000000"
        )
    );
}

#[test]
fn test_builtin_rightpad_func_sig() {
    let source: &str = r#"
        #define macro MAIN() = takes (0) returns (0) {
            __RIGHTPAD(__FUNC_SIG('transfer(address,uint256)'))
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
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    // PUSH32 = 0x7f, transfer(address,uint256) signature = 0xa9059cbb
    assert_eq!(&r_bytes, "7fa9059cbb00000000000000000000000000000000000000000000000000000000");
}

#[test]
fn test_builtin_rightpad_bytes() {
    let source: &str = r#"
        #define macro MAIN() = takes (0) returns (0) {
            __RIGHTPAD(__BYTES('hello'))
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
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    // PUSH32 = 0x7f, "hello" = 0x68656c6c6f
    assert_eq!(&r_bytes, "7f68656c6c6f000000000000000000000000000000000000000000000000000000");
}

#[test]
fn test_bytes_builtin() {
    let source: &str = r#"
        #define macro MAIN() = takes (0) returns (0) {
            __BYTES("hello")
            __BYTES("hellohello")
            __BYTES("🙂🙂") // Will be represended as UTF-8 4-byte
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let contract = parser.parse().unwrap();

    // Instantiate Codegen
    let cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    // Have Codegen create the runtime bytecode
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    // 64 = PUSH5, "hello" = 0x68656c6c6f
    assert_eq!(&r_bytes[0..12], "6468656c6c6f"); // 2 + 2 * 5 chars
    // 69 = PUSH10, "hellohello" = 0x68656c6c6f68656c6c6f
    assert_eq!(&r_bytes[12..34], "6968656c6c6f68656c6c6f"); // 2 + 2 * 10 chars
    // 67 = PUSH8, "🙂🙂" = 0xf09f9982f09f9982
    assert_eq!(&r_bytes[34..52], "67f09f9982f09f9982"); // 2 + 8 * 2 chars
    assert_eq!(r_bytes.len(), (2 + 5 * 2) + (2 + 2 * 10) + (2 + 8 * 2));
}

#[test]
fn test_bytes_builtin_too_large_error() {
    let source: &str = r#"
        #define macro MAIN() = takes (0) returns (0) {
            __BYTES("hellohellohellohellohellohellohello") // 35 characters, codegen will fail
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let contract = parser.parse().unwrap();

    // Instantiate Codegen
    let cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::InvalidArguments(String::from("Encoded bytes length exceeds 32 bytes")));
}

#[test]
fn test_bytes_builtin_with_hex_constant() {
    let source: &str = r#"
        #define constant MY_HEX = 0x1234

        #define macro MAIN() = takes (0) returns (0) {
            __BYTES([MY_HEX])
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen should fail - __BYTES requires string constants, not hex constants
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(codegen_result.is_err());
    let err = codegen_result.unwrap_err();
    assert!(matches!(err.kind, CodegenErrorKind::InvalidArguments(_)));
}

#[test]
fn test_bytes_builtin_empty_string_error() {
    let source: &str = r#"
        #define macro MAIN() = takes (0) returns (0) {
            __BYTES("")
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let contract = parser.parse().unwrap();

    // Instantiate Codegen
    let cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::InvalidArguments(String::from("Empty string passed to __BYTES")));
}

#[test]
fn test_bytes_builtin_with_string_constant() {
    let source: &str = r#"
        #define constant GREETING = "hello"

        #define macro MAIN() = takes (0) returns (0) {
            __BYTES([GREETING])
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Generate runtime bytecode
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // "hello" = 0x68656c6c6f (UTF-8 bytes)
    // 64 = PUSH5, value = 0x68656c6c6f
    assert_eq!(&r_bytes[0..12], "6468656c6c6f");
}

#[test]
fn test_string_constant_cannot_be_pushed_directly() {
    let source: &str = r#"
        #define constant GREETING = "hello"

        #define macro MAIN() = takes (0) returns (0) {
            [GREETING]
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(codegen_result.is_err());
    let err = codegen_result.unwrap_err();
    assert!(matches!(err.kind, CodegenErrorKind::StringConstantNotBytes(_)));
}

mod common;

#[test]
fn test_error_builtin() {
    let source: &str = r#"
        #define error CustomError()

        #define macro MAIN() = takes(0) returns(0) {
            __ERROR(CustomError)
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
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    // __ERROR with error definition: PUSH32 + selector right-padded to 32 bytes
    // keccak256("CustomError()")[0:4] = 0x09caebf3
    // 7f = PUSH32
    assert_eq!(r_bytes, "7f09caebf300000000000000000000000000000000000000000000000000000000");
}

#[test]
fn test_assert_pc_success() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            0x01 0x02       // 4 bytes total (PUSH1 0x01, PUSH1 0x02)
            __ASSERT_PC(0x04)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Expected bytecode: PUSH1 0x01, PUSH1 0x02 = 0x6001 0x6002
    assert_eq!(bytecode, "60016002");
}

#[test]
fn test_assert_pc_with_constant() {
    let source: &str = r#"
        #define constant TARGET_PC = 0x04
        #define macro MAIN() = takes(0) returns(0) {
            0x01 0x02       // 4 bytes total
            __ASSERT_PC([TARGET_PC])
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60016002");
}

#[test]
fn test_assert_pc_with_arithmetic_expression() {
    let source: &str = r#"
        #define constant BASE = 0x02
        #define constant OFFSET = 0x02
        #define constant TARGET_PC = [BASE] + [OFFSET]
        #define macro MAIN() = takes(0) returns(0) {
            0x01 0x02       // 4 bytes total
            __ASSERT_PC([TARGET_PC])  // Should evaluate to 0x04
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60016002");
}

#[test]
fn test_assert_pc_failure() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x01 0x02       // 4 bytes total
            __ASSERT_PC(0x00)  // Wrong! Should be 0x04
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::AssertPcFailed(0, 4)));
}

#[test]
fn test_assert_pc_complex_positioning() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            0x01            // PUSH1 0x01 = 2 bytes
            __ASSERT_PC(0x02)
            0x02 0x03       // PUSH1 0x02, PUSH1 0x03 = 4 bytes
            __ASSERT_PC(0x06)
            jumpdest        // JUMPDEST = 1 byte
            __ASSERT_PC(0x07)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH1 0x01, PUSH1 0x02, PUSH1 0x03, JUMPDEST
    assert_eq!(bytecode, "6001600260035b");
}

#[test]
fn test_assert_pc_with_label() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            0x01 0x02       // PUSH1 0x01, PUSH1 0x02 = 4 bytes
            __ASSERT_PC(0x04)
            target:         // Label generates JUMPDEST = 1 byte (position 4)
                0x03        // PUSH1 0x03 = 2 bytes (position 5)
            __ASSERT_PC(0x07)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH1 0x01, PUSH1 0x02, JUMPDEST, PUSH1 0x03
    assert_eq!(bytecode, "600160025b6003");
}

#[test]
fn test_assert_pc_with_macro_call() {
    let source: &str = r#"
        #define macro HELPER() = takes(0) returns(0) {
            0x01 0x02       // PUSH1 0x01, PUSH1 0x02 = 4 bytes
        }

        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            HELPER()        // 4 bytes
            __ASSERT_PC(0x04)
            0x03            // PUSH1 0x03 = 2 bytes
            __ASSERT_PC(0x06)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH1 0x01, PUSH1 0x02, PUSH1 0x03
    assert_eq!(bytecode, "600160026003");
}

#[test]
fn test_assert_pc_with_label_and_jump() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            target jumpi    // PUSH2 target, JUMPI = 4 bytes
            __ASSERT_PC(0x04)
            0x01            // PUSH1 0x01 = 2 bytes (position 4-5)
            __ASSERT_PC(0x06)
            target:         // Label generates JUMPDEST = 1 byte (position 6)
                stop        // STOP = 1 byte (position 7)
            __ASSERT_PC(0x08)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH2 0x0006, JUMPI, PUSH1 0x01, JUMPDEST, STOP
    assert_eq!(bytecode, "6100065760015b00");
}

#[test]
fn test_assert_pc_nested_macro_calls() {
    let source: &str = r#"
        #define macro INNER() = takes(0) returns(0) {
            0x01            // PUSH1 0x01 = 2 bytes
        }

        #define macro MIDDLE() = takes(0) returns(0) {
            INNER()         // 2 bytes
            0x02            // PUSH1 0x02 = 2 bytes
        }

        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            MIDDLE()        // 4 bytes
            __ASSERT_PC(0x04)
            0x03            // PUSH1 0x03 = 2 bytes
            __ASSERT_PC(0x06)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH1 0x01, PUSH1 0x02, PUSH1 0x03
    assert_eq!(bytecode, "600160026003");
}

#[test]
fn test_assert_pc_with_multiple_labels() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            label1:         // Label generates JUMPDEST = 1 byte (position 0)
                0x01        // PUSH1 0x01 = 2 bytes (position 1-2)

            __ASSERT_PC(0x03)
            label2:         // Label generates JUMPDEST = 1 byte (position 3)
                0x02        // PUSH1 0x02 = 2 bytes (position 4-5)
                __ASSERT_PC(0x06)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // JUMPDEST, PUSH1 0x01, JUMPDEST, PUSH1 0x02
    assert_eq!(bytecode, "5b60015b6002");
}

#[test]
fn test_assert_pc_with_jump_and_multiple_labels() {
    let source: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
            dup1 case_true jumpi    // DUP1, PUSH2 case_true, JUMPI = 1 + 3 + 1 = 5 bytes (0-4)

            __ASSERT_PC(0x05)
            case_false:             // JUMPDEST = 1 byte (position 5)
                0x11                // PUSH1 0x11 = 2 bytes (6-7)
                end jump            // PUSH2 end, JUMP = 4 bytes (8-11)

            __ASSERT_PC(0x0C)
            case_true:              // JUMPDEST = 1 byte (position 12)
                0x22                // PUSH1 0x22 = 2 bytes (13-14)

            __ASSERT_PC(0x0F)
            end:                    // JUMPDEST = 1 byte (position 15)
                stop                // STOP = 1 byte (position 16)
            __ASSERT_PC(0x11)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Expected bytecode:
    // 80 = DUP1
    // 61 000C 57 = PUSH2 0x000C (case_true), JUMPI
    // 5B = JUMPDEST (case_false)
    // 60 11 = PUSH1 0x11
    // 61 000F 56 = PUSH2 0x000F (end), JUMP
    // 5B = JUMPDEST (case_true)
    // 60 22 = PUSH1 0x22
    // 5B = JUMPDEST (end)
    // 00 = STOP
    assert_eq!(bytecode, "8061000c575b601161000f565b60225b00");
}

#[test]
fn test_assert_pc_with_jump_relaxation() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            target jump
            target:
                __ASSERT_PC(0x04)
                stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Without relaxation: PUSH2 0x0005 (3 bytes) + JUMP (1 byte) + JUMPDEST (1 byte) = __ASSERT_PC at 0x05
    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);
    // Should fail because __ASSERT_PC expects 0x04 but actual PC is 0x05
    assert!(bytecode_without.is_err(), "Without relaxation, __ASSERT_PC(0x04) should fail (PC is 0x05)");

    // With relaxation: PUSH1 0x04 (2 bytes) + JUMP (1 byte) + JUMPDEST (1 byte) = __ASSERT_PC at 0x04
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true);
    // After relaxation, __ASSERT_PC validates using post-relaxation offsets
    assert!(bytecode_with.is_ok(), "With relaxation, __ASSERT_PC(0x04) should succeed (PC is 0x04)");

    // Verify the bytecode is correct: 60 03 56 5b 00 (PUSH1 0x03, JUMP, JUMPDEST, STOP)
    // The jump target is 0x03 (JUMPDEST), even though __ASSERT_PC checks position 0x04 (after JUMPDEST)
    let bc = bytecode_with.unwrap();
    assert_eq!(bc, "6003565b00", "Should be PUSH1 0x03, JUMP, JUMPDEST, STOP");
}
