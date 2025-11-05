mod common;

#[test]
fn test_rightpad_in_constant_reference() {
    let source = r#"
        #define constant C1 = __RIGHTPAD(0x)
        #define constant C2 = [C1]

        #define macro MAIN() = takes(0) returns(0) {
            [C2]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // __RIGHTPAD(0x) produces 32 zero bytes, optimized to PUSH1 0x00
    assert_eq!(bytecode, "5f"); // PUSH0 (0x5f) in Shanghai+ or PUSH1 0x00 in earlier versions
}

#[test]
fn test_func_sig_in_constant_reference() {
    let source = r#"
        #define constant SIG = __FUNC_SIG("transfer(address,uint256)")
        #define constant SELECTOR = [SIG]

        #define macro MAIN() = takes(0) returns(0) {
            [SELECTOR]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // transfer(address,uint256) signature = 0xa9059cbb
    // PUSH4 = 0x63
    assert_eq!(bytecode, "63a9059cbb");
}

#[test]
fn test_event_hash_in_constant_reference() {
    let source = r#"
        #define constant HASH = __EVENT_HASH("Transfer(address,address,uint256)")
        #define constant EVENT_TOPIC = [HASH]

        #define macro MAIN() = takes(0) returns(0) {
            [EVENT_TOPIC]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // Transfer(address,address,uint256) event hash
    // keccak256("Transfer(address,address,uint256)") = 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
    // PUSH32 = 0x7f
    assert_eq!(bytecode, "7fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef");
}

#[test]
fn test_bytes_in_constant_reference() {
    let source = r#"
        #define constant B = __BYTES("hello")
        #define constant BYTES_VAL = [B]

        #define macro MAIN() = takes(0) returns(0) {
            [BYTES_VAL]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // "hello" as bytes, right-padded to 32 bytes: 0x68656c6c6f (followed by zeros)
    // Since it's only 5 bytes, it will be optimized to PUSH5
    // PUSH5 = 0x64
    assert_eq!(bytecode, "6468656c6c6f");
}

#[test]
fn test_nested_constant_references_with_builtin() {
    let source = r#"
        #define constant C1 = __FUNC_SIG("test()")
        #define constant C2 = [C1]
        #define constant C3 = [C2]

        #define macro MAIN() = takes(0) returns(0) {
            [C3]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // test() signature = keccak256("test()")[0..4] = 0xf8a8fd6d
    // PUSH4 = 0x63
    assert_eq!(bytecode, "63f8a8fd6d");
}

#[test]
fn test_builtin_in_arithmetic_expression() {
    let source = r#"
        #define constant SIG = __FUNC_SIG("test()")
        #define constant OFFSET = [SIG] + 1

        #define macro MAIN() = takes(0) returns(0) {
            [OFFSET]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // test() signature = 0xf8a8fd6d = 4171875693
    // 4171875693 + 1 = 4171875694 = 0xf8a8fd6e
    // PUSH4 = 0x63
    assert_eq!(bytecode, "63f8a8fd6e");
}

#[test]
fn test_leftpad_in_constant_reference() {
    let source = r#"
        #define constant PADDED = __LEFTPAD(0x42)
        #define constant VAL = [PADDED]

        #define macro MAIN() = takes(0) returns(0) {
            [VAL]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // __LEFTPAD(0x42) produces 0x0000...0042 (left-padded to 32 bytes)
    // This is just 0x42, which gets optimized to PUSH1
    // PUSH1 = 0x60
    assert_eq!(bytecode, "6042");
}

#[test]
fn test_rightpad_vs_leftpad() {
    let source_right = r#"
        #define constant PADDED = __RIGHTPAD(0x42)
        #define constant VAL = [PADDED]

        #define macro MAIN() = takes(0) returns(0) {
            [VAL]
        }
    "#;

    let bytecode_right = common::compile_to_bytecode(source_right).unwrap();
    // __RIGHTPAD(0x42) produces 0x4200...0000 (right-padded to 32 bytes)
    // This is 0x42 followed by 31 zero bytes
    // PUSH32 = 0x7f (full 32 bytes)
    assert_eq!(bytecode_right, "7f4200000000000000000000000000000000000000000000000000000000000000");

    let source_left = r#"
        #define constant PADDED = __LEFTPAD(0x42)
        #define constant VAL = [PADDED]

        #define macro MAIN() = takes(0) returns(0) {
            [VAL]
        }
    "#;

    let bytecode_left = common::compile_to_bytecode(source_left).unwrap();
    // __LEFTPAD(0x42) produces 0x0000...0042 (left-padded to 32 bytes)
    // Leading zeros are trimmed, so it's just 0x42
    // PUSH1 = 0x60
    assert_eq!(bytecode_left, "6042");
}

#[test]
fn test_multiple_builtins_in_constants() {
    let source = r#"
        #define constant FUNC_SIG = __FUNC_SIG("transfer(address,uint256)")
        #define constant EVENT_HASH = __EVENT_HASH("Transfer(address,address,uint256)")
        #define constant SIG_REF = [FUNC_SIG]
        #define constant HASH_REF = [EVENT_HASH]

        #define macro MAIN() = takes(0) returns(0) {
            [SIG_REF]
            [HASH_REF]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // transfer(address,uint256) = 0xa9059cbb (PUSH4)
    // Transfer(address,address,uint256) = 0xddf252ad... (PUSH32)
    assert_eq!(bytecode, "63a9059cbb7fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef");
}

#[test]
fn test_error_selector_in_constant_reference() {
    let source = r#"
        #define error CustomError(uint256)

        #define constant ERR = __FUNC_SIG(CustomError)
        #define constant ERR_REF = [ERR]

        #define macro MAIN() = takes(0) returns(0) {
            [ERR_REF]
        }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    // CustomError(uint256) selector = keccak256("CustomError(uint256)")[0..4] = 0x110b3655
    // PUSH4 = 0x63
    assert_eq!(bytecode, "63110b3655");
}
