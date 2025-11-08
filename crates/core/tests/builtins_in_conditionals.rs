mod common;

use common::compile_to_bytecode;

#[test]
fn test_rightpad_in_if_statement() {
    // Test that __RIGHTPAD builtin works inside if-statement body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (true) {
                __RIGHTPAD(0x1234)
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected: PUSH32 with right-padded 0x1234
    assert_eq!(bytecode.len(), 66); // 7f (PUSH32) + 64 hex chars (32 bytes)
    assert!(bytecode.starts_with("7f1234"));
}

#[test]
fn test_func_sig_in_if_statement() {
    // Test that __FUNC_SIG builtin works inside if-statement body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (true) {
                __FUNC_SIG(transfer)
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected: PUSH4 with function selector for transfer()
    assert_eq!(bytecode, "63b483afd3");
}

#[test]
fn test_event_hash_in_if_statement() {
    // Test that __EVENT_HASH builtin works inside if-statement body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (true) {
                __EVENT_HASH(Transfer)
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected: PUSH32 with event hash for Transfer()
    assert_eq!(bytecode.len(), 66); // 7f (PUSH32) + 64 hex chars
    assert!(bytecode.starts_with("7f"));
}

#[test]
fn test_nested_if_with_builtins() {
    // Test that builtins work in nested if-statements
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (true) {
                if (false) {
                    __RIGHTPAD(0x1234)
                }
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected: empty (false condition means inner block not compiled)
    assert_eq!(bytecode, "");
}

#[test]
fn test_multiple_builtins_in_if() {
    // Test multiple builtin calls in the same if-statement body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (true) {
                __FUNC_SIG(transfer)
                __RIGHTPAD(0xabcd)
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected: PUSH4 for __FUNC_SIG followed by PUSH32 for __RIGHTPAD
    assert_eq!(bytecode, "63b483afd37fabcd000000000000000000000000000000000000000000000000000000000000");
}
