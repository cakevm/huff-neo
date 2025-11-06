mod common;

use common::compile_to_bytecode;

#[test]
fn test_arg_in_if_condition_true() {
    // Test if statement with <arg> in condition (true case)
    let source = r#"
        #define macro TEST(flag) = takes(0) returns(0) {
            if(<flag>) {
                0x42
            } else {
                0x10
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x01)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x42 (then branch only, since 0x01 is true)
    let expected = "6042";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_arg_in_if_condition_false() {
    // Test if statement with <arg> in condition (false case)
    let source = r#"
        #define macro TEST(flag) = takes(0) returns(0) {
            if(<flag>) {
                0x42
            } else {
                0x10
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x00)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x10 (else branch only, since 0x00 is false)
    let expected = "6010";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_arg_in_for_loop_bounds() {
    // Test for loop with <arg> in upper bound
    let source = r#"
        #define macro LOOP(count) = takes(0) returns(0) {
            for(i in 0..<count>) {
                <i>
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            LOOP(0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0, PUSH1 0x01, PUSH1 0x02 (loop unrolled with count=3)
    // 5f 6001 6002
    let expected = "5f60016002";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_multiple_args_in_if() {
    // Test multiple <arg> parameters
    let source = r#"
        #define macro TEST(flag1, flag2) = takes(0) returns(0) {
            if(<flag1>) {
                0xFF
            } else {
                0x11
            }
            if(<flag2>) {
                0xAA
            } else {
                0xBB
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x01, 0x00)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (flag1=true), PUSH1 0xBB (flag2=false)
    let expected = "60ff60bb";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_nested_arg_in_if() {
    // Test <arg> passed through nested macro invocations
    let source = r#"
        #define macro INNER(val) = takes(0) returns(0) {
            if(<val>) {
                0x99
            } else {
                0x11
            }
        }

        #define macro OUTER(flag) = takes(0) returns(0) {
            INNER(<flag>)
        }

        #define macro MAIN() = takes(0) returns(0) {
            OUTER(0x01)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x99 (nested arg resolves to 0x01, true branch)
    let expected = "6099";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_arg_in_for_loop_with_step() {
    // Test for loop with <arg> in upper bound and step
    let source = r#"
        #define macro LOOP(count) = takes(0) returns(0) {
            for(i in 0..<count> step 2) {
                <i>
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            LOOP(0x06)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0, PUSH1 0x02, PUSH1 0x04
    // Loop from 0 to 6 with step 2: 0, 2, 4
    let expected = "5f60026004";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_arg_in_else_if_condition() {
    // Test <arg> in else-if conditions (using non-zero as true)
    let source = r#"
        #define macro TEST(val) = takes(0) returns(0) {
            if(<val>) {
                0xAA
            } else if(!<val>) {
                0xBB
            } else {
                0xCC
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xAA (val is 0x02, non-zero so first condition is true)
    let expected = "60aa";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_arg_passed_to_nested_macro_in_for() {
    // Test <arg> passed through multiple macro levels in for loop
    let source = r#"
        #define macro LOOP(n) = takes(0) returns(0) {
            for(i in 0..<n>) {
                0xFF
            }
        }

        #define macro OUTER(count) = takes(0) returns(0) {
            LOOP(<count>)
        }

        #define macro MAIN() = takes(0) returns(0) {
            OUTER(0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF, PUSH1 0xFF (loop runs twice)
    let expected = "60ff60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_arg_in_nested_for_loops() {
    // Test <arg> in nested for loop bounds
    let source = r#"
        #define macro NESTED(outer_count) = takes(0) returns(0) {
            for(i in 0..<outer_count>) {
                for(j in 0..0x02) {
                    <i>
                    <j>
                }
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            NESTED(0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode:
    // Outer loop i=0: j=0 -> (0,0), j=1 -> (0,1)
    // Outer loop i=1: j=0 -> (1,0), j=1 -> (1,1)
    // 5f5f 5f6001 60015f 60016001
    let expected = "5f5f5f60016001 5f60016001";
    assert_eq!(bytecode.to_lowercase(), expected.replace(" ", "").to_lowercase());
}
