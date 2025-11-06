mod common;

use crate::common::compile_to_bytecode;

// Tests for arithmetic operations with <arg> in if/for expressions
// These mirror the tests in arithmetic_expressions.rs but use macro arguments

// Basic Arithmetic Operations

#[test]
fn test_arg_simple_addition() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> + <b>) {  // 0x10 + 0x05 = 0x15 (true)
                0x42
            } else {
                0x10
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x10, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6042", "0x10 + 0x05 = 0x15 (non-zero, true branch)");
}

#[test]
fn test_arg_simple_subtraction() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> - <b>) {  // 0x20 - 0x08 = 0x18 (true)
                0xAA
            } else {
                0xBB
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x20, 0x08)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60aa", "0x20 - 0x08 = 0x18 (non-zero, true branch)");
}

#[test]
fn test_arg_simple_multiplication() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> * <b>) {  // 0x03 * 0x07 = 0x15 (true)
                0xFF
            } else {
                0x00
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x03, 0x07)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ff", "0x03 * 0x07 = 0x15 (non-zero, true branch)");
}

#[test]
fn test_arg_simple_division() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> / <b>) {  // 0x64 / 0x0a = 0x0a (true)
                0x55
            } else {
                0x66
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x64, 0x0a)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6055", "0x64 / 0x0a = 0x0a (non-zero, true branch)");
}

#[test]
fn test_arg_simple_modulo() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> % <b>) {  // 0x0a % 0x03 = 0x01 (true)
                0x77
            } else {
                0x88
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0a, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6077", "0x0a % 0x03 = 0x01 (non-zero, true branch)");
}

#[test]
fn test_arg_unary_negation() {
    let source = r#"
        #define macro TEST(a) = takes(0) returns(0) {
            if(-<a>) {  // -5 is non-zero (true)
                0x99
            } else {
                0x11
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6099", "-0x05 is non-zero (true branch)");
}

// Operator Precedence

#[test]
fn test_arg_operator_precedence_mul_before_add() {
    let source = r#"
        #define macro TEST(a, b, c) = takes(0) returns(0) {
            if(<a> + <b> * <c>) {  // 2 + (3 * 4) = 14 (true)
                0xCC
            } else {
                0xDD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02, 0x03, 0x04)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60cc", "2 + (3 * 4) = 14 (true)");
}

#[test]
fn test_arg_operator_precedence_div_before_sub() {
    let source = r#"
        #define macro TEST(a, b, c) = takes(0) returns(0) {
            if(<a> - <b> / <c>) {  // 10 - (8 / 2) = 6 (true)
                0xEE
            } else {
                0xFF
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0a, 0x08, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ee", "10 - (8 / 2) = 6 (true)");
}

#[test]
fn test_arg_parenthesized_expression_override_precedence() {
    let source = r#"
        #define macro TEST(a, b, c) = takes(0) returns(0) {
            if((<a> + <b>) * <c>) {  // (2 + 3) * 4 = 20 (true)
                0xAB
            } else {
                0xCD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02, 0x03, 0x04)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ab", "(2 + 3) * 4 = 20 (true)");
}

#[test]
fn test_arg_nested_parentheses() {
    let source = r#"
        #define macro TEST(a, b, c, d) = takes(0) returns(0) {
            if(((<a> + <b>) * <c>) - <d>) {  // ((2 + 3) * 4) - 5 = 15 (true)
                0xBA
            } else {
                0xDC
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02, 0x03, 0x04, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ba", "((2 + 3) * 4) - 5 = 15 (true)");
}

// Associativity

#[test]
fn test_arg_left_associativity_subtraction() {
    let source = r#"
        #define macro TEST(a, b, c) = takes(0) returns(0) {
            if(<a> - <b> - <c>) {  // (10 - 3) - 2 = 5 (true)
                0x11
            } else {
                0x22
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0a, 0x03, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6011", "(10 - 3) - 2 = 5 (true)");
}

#[test]
fn test_arg_left_associativity_division() {
    let source = r#"
        #define macro TEST(a, b, c) = takes(0) returns(0) {
            if(<a> / <b> / <c>) {  // (20 / 4) / 2 = 2 (true)
                0x33
            } else {
                0x44
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x14, 0x04, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6033", "(20 / 4) / 2 = 2 (true)");
}

// Complex Expressions

#[test]
fn test_arg_complex_expression() {
    let source = r#"
        #define macro TEST(a, b, c, d, e) = takes(0) returns(0) {
            if(<a> * <b> + <c> - <d> / <e>) {  // 3*4 + 10 - 6/2 = 19 (true)
                0xFE
            } else {
                0xED
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x03, 0x04, 0x0a, 0x06, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60fe", "3*4 + 10 - 6/2 = 19 (true)");
}

#[test]
fn test_arg_all_operators_combined() {
    let source = r#"
        #define macro TEST(a, b, c, d, e, f) = takes(0) returns(0) {
            if(<a> + <b> * <c> - <d> / <e> % <f>) {  // Complex expression
                0xCA
            } else {
                0xFE
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05, 0x03, 0x04, 0x08, 0x02, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ca", "Complex expression evaluates to non-zero");
}

// Special Cases

#[test]
fn test_arg_double_negation() {
    let source = r#"
        #define macro TEST(a) = takes(0) returns(0) {
            if(-(-<a>)) {  // --5 = 5 (true)
                0xBE
            } else {
                0xEF
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60be", "--5 = 5 (true)");
}

#[test]
fn test_arg_negation_in_expression() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> + (-<b>)) {  // 10 + (-5) = 5 (true)
                0xDE
            } else {
                0xAD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0a, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60de", "10 + (-5) = 5 (true)");
}

#[test]
fn test_arg_zero_result_takes_else_branch() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> - <b>) {  // 5 - 5 = 0 (false)
                0x11
            } else {
                0x22
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6022", "5 - 5 = 0 (zero is false, else branch)");
}

#[test]
fn test_arg_multiplication_by_zero() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> * <b>) {  // 10 * 0 = 0 (false)
                0xAA
            } else {
                0xBB
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0a, 0x00)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60bb", "10 * 0 = 0 (false, else branch)");
}

// Arg in For Loops

#[test]
fn test_arg_in_for_loop_bound_with_arithmetic() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            for(i in 0..(<a> + <b>)) {  // 0..(2+3) = 0..5
                <i>
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Expected: PUSH0, PUSH1 0x01, PUSH1 0x02, PUSH1 0x03, PUSH1 0x04
    assert_eq!(bytecode, "5f6001600260036004", "Loop from 0 to 5");
}

#[test]
fn test_arg_in_for_loop_multiplication() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            for(i in 0..(<a> * <b>)) {  // 0..(2*3) = 0..6
                0xFF
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Expected: six PUSH1 0xFF
    assert_eq!(bytecode, "60ff60ff60ff60ff60ff60ff", "Loop runs 6 times");
}

// Mixed Args and Constants

#[test]
fn test_arg_with_literal() {
    let source = r#"
        #define macro TEST(a) = takes(0) returns(0) {
            if(<a> + 0x10) {  // arg + literal
                0xCA
            } else {
                0xFE
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ca", "arg + literal works");
}

#[test]
fn test_arg_with_constant() {
    let source = r#"
        #define constant BASE = 0x10

        #define macro TEST(a) = takes(0) returns(0) {
            if(<a> + [BASE]) {  // arg + constant
                0xAB
            } else {
                0xBA
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ab", "arg + constant works");
}

// Comparison Operators

#[test]
fn test_arg_equal_comparison() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> == <b>) {  // 5 == 5 (true, result is 1)
                0x55
            } else {
                0x66
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6055", "5 == 5 is true");
}

#[test]
fn test_arg_not_equal_comparison() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> != <b>) {  // 5 != 3 (true, result is 1)
                0x77
            } else {
                0x88
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6077", "5 != 3 is true");
}

#[test]
fn test_arg_less_than_comparison() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> < <b>) {  // 3 < 5 (true, result is 1)
                0x99
            } else {
                0xAA
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x03, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6099", "3 < 5 is true");
}

#[test]
fn test_arg_greater_than_comparison() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> > <b>) {  // 7 > 3 (true, result is 1)
                0xBB
            } else {
                0xCC
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x07, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60bb", "7 > 3 is true");
}

#[test]
fn test_arg_less_equal_comparison() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> <= <b>) {  // 5 <= 5 (true, result is 1)
                0xDD
            } else {
                0xEE
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60dd", "5 <= 5 is true");
}

#[test]
fn test_arg_greater_equal_comparison() {
    let source = r#"
        #define macro TEST(a, b) = takes(0) returns(0) {
            if(<a> >= <b>) {  // 7 >= 5 (true, result is 1)
                0xFF
            } else {
                0x00
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x07, 0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ff", "7 >= 5 is true");
}

// Logical NOT

#[test]
fn test_arg_logical_not() {
    let source = r#"
        #define macro TEST(a) = takes(0) returns(0) {
            if(!<a>) {  // !0 = true (1)
                0x12
            } else {
                0x34
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x00)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6012", "!0 is true");
}

#[test]
fn test_arg_logical_not_nonzero() {
    let source = r#"
        #define macro TEST(a) = takes(0) returns(0) {
            if(!<a>) {  // !5 = false (0)
                0x56
            } else {
                0x78
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6078", "!5 is false");
}
