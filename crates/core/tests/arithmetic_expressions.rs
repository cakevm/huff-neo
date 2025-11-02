mod common;

use crate::common::{assert_compile_error, compile_to_bytecode, compile_to_constructor_bytecode};
use huff_neo_utils::error::CodegenErrorKind;

// Basic Arithmetic Operations

#[test]
fn test_simple_addition() {
    let source = r#"
        #define constant A = 0x10
        #define constant B = 0x05
        #define constant RESULT = A + B  // 0x15

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6015", "Expected PUSH1 0x15 (21)");
}

#[test]
fn test_simple_subtraction() {
    let source = r#"
        #define constant A = 0x20
        #define constant B = 0x08
        #define constant RESULT = A - B  // 0x18

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6018", "Expected PUSH1 0x18 (24)");
}

#[test]
fn test_simple_multiplication() {
    let source = r#"
        #define constant A = 0x03
        #define constant B = 0x07
        #define constant RESULT = A * B  // 0x15

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6015", "Expected PUSH1 0x15 (21)");
}

#[test]
fn test_simple_division() {
    let source = r#"
        #define constant A = 0x64  // 100
        #define constant B = 0x0a  // 10
        #define constant RESULT = A / B  // 0x0a

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "600a", "Expected PUSH1 0x0a (10)");
}

#[test]
fn test_simple_modulo() {
    let source = r#"
        #define constant A = 0x0a  // 10
        #define constant B = 0x03  // 3
        #define constant RESULT = A % B  // 0x01

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6001", "Expected PUSH1 0x01 (1)");
}

#[test]
fn test_unary_negation() {
    let source = r#"
        #define constant A = 0x05
        #define constant RESULT = -A  // Two's complement of 5

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Two's complement of 5: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB
    assert_eq!(bytecode, "7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb", "Expected two's complement of 5");
}

// Operator Precedence and Grouping

#[test]
fn test_operator_precedence_mul_before_add() {
    let source = r#"
        #define constant A = 0x02
        #define constant B = 0x03
        #define constant C = 0x04
        #define constant RESULT = A + B * C  // 2 + (3 * 4) = 14 = 0x0e

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "600e", "Expected PUSH1 0x0e (14) - multiplication before addition");
}

#[test]
fn test_operator_precedence_div_before_sub() {
    let source = r#"
        #define constant RESULT = 0x14 - 0x0c / 0x03  // 20 - (12 / 3) = 20 - 4 = 16 = 0x10

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6010", "Expected PUSH1 0x10 (16) - division before subtraction");
}

#[test]
fn test_operator_precedence_mod_before_add() {
    let source = r#"
        #define constant RESULT = 0x0a + 0x0d % 0x05  // 10 + (13 % 5) = 10 + 3 = 13 = 0x0d

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "600d", "Expected PUSH1 0x0d (13) - modulo before addition");
}

#[test]
fn test_parenthesized_expression_override_precedence() {
    let source = r#"
        #define constant A = 0x02
        #define constant B = 0x03
        #define constant C = 0x04
        #define constant RESULT = (A + B) * C  // (2 + 3) * 4 = 20 = 0x14

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6014", "Expected PUSH1 0x14 (20) - parentheses override precedence");
}

#[test]
fn test_nested_parentheses() {
    let source = r#"
        #define constant A = 0x01
        #define constant B = 0x02
        #define constant C = 0x03
        #define constant D = 0x04
        #define constant RESULT = ((A + B) * (C + D))  // (1+2) * (3+4) = 3 * 7 = 21 = 0x15

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6015", "Expected PUSH1 0x15 (21) - nested parentheses");
}

#[test]
fn test_deeply_nested_parentheses() {
    let source = r#"
        #define constant A = 0x05
        #define constant RESULT = ((((A))))  // Should just be 5

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6005", "Expected PUSH1 0x05 (5) - deeply nested parentheses");
}

// Left Associativity

#[test]
fn test_left_associativity_subtraction() {
    let source = r#"
        #define constant RESULT = 0x0a - 0x03 - 0x02  // (10 - 3) - 2 = 7 - 2 = 5 = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6005", "Expected PUSH1 0x05 (5) - left-associative subtraction");
}

#[test]
fn test_left_associativity_division() {
    let source = r#"
        #define constant RESULT = 0x64 / 0x0a / 0x02  // (100 / 10) / 2 = 10 / 2 = 5 = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6005", "Expected PUSH1 0x05 (5) - left-associative division");
}

#[test]
fn test_left_associativity_mixed_same_precedence() {
    let source = r#"
        #define constant RESULT = 0x14 - 0x05 + 0x03  // (20 - 5) + 3 = 15 + 3 = 18 = 0x12

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6012", "Expected PUSH1 0x12 (18) - left-to-right evaluation");
}

// Complex and Mixed Expressions

#[test]
fn test_complex_expression_with_constants() {
    let source = r#"
        #define constant BASE = 0x20
        #define constant OFFSET = 0x04
        #define constant MULTIPLIER = 0x02
        #define constant RESULT = BASE + (OFFSET * MULTIPLIER)  // 32 + (4 * 2) = 40 = 0x28

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6028", "Expected PUSH1 0x28 (40)");
}

#[test]
fn test_all_operators_combined() {
    let source = r#"
        #define constant A = 0x0a  // 10
        #define constant B = 0x03  // 3
        #define constant C = 0x02  // 2
        #define constant RESULT = ((A + B) * C - 0x04) / 0x05  // ((10+3)*2-4)/5 = (26-4)/5 = 22/5 = 4

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6004", "Expected PUSH1 0x04 (4) - all operators combined");
}

#[test]
fn test_mixed_precedence_levels() {
    let source = r#"
        #define constant RESULT = 0x02 + 0x03 * 0x04 - 0x08 / 0x02  // 2 + (3*4) - (8/2) = 2 + 12 - 4 = 10 = 0x0a

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "600a", "Expected PUSH1 0x0a (10) - mixed precedence levels");
}

// Constant References and Chaining

#[test]
fn test_chained_constant_references() {
    let source = r#"
        #define constant A = 0x01
        #define constant B = A + 0x01  // 2
        #define constant C = B + 0x01  // 3
        #define constant D = C + 0x01  // 4
        #define constant RESULT = A + B + C + D  // 1 + 2 + 3 + 4 = 10 = 0x0a

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "600a", "Expected PUSH1 0x0a (10) - chained constant references");
}

#[test]
fn test_hex_literal_in_expression() {
    let source = r#"
        #define constant RESULT = 0x10 + 0x05  // 16 + 5 = 21 = 0x15

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6015", "Expected PUSH1 0x15 (21) - hex literals in expressions");
}

// Edge Cases

#[test]
fn test_double_negation() {
    let source = r#"
        #define constant A = 0x05
        #define constant NEG_A = -A
        #define constant RESULT = -NEG_A  // --5 = 5

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6005", "Expected PUSH1 0x05 (5) - double negation");
}

#[test]
fn test_negation_in_expression() {
    let source = r#"
        #define constant A = 0x05
        #define constant B = 0x03
        #define constant RESULT = B + -A  // 3 + (-5) - tests negation as operand

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Result is wrapping addition: 3 + two's_complement(5)
    assert_eq!(bytecode, "7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe", "Expected wrapped result of 3 + (-5)");
}

#[test]
fn test_zero_operations() {
    let source = r#"
        #define constant ZERO = 0x00
        #define constant ADD_ZERO = 0x05 + ZERO  // 5
        #define constant SUB_ZERO = 0x05 - ZERO  // 5
        #define constant MUL_ZERO = 0x05 * ZERO  // 0

        #define macro MAIN() = takes(0) returns(0) {
            [ADD_ZERO]
            [SUB_ZERO]
            [MUL_ZERO]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // Note: 0x5f is PUSH0 in newer EVM versions
    assert_eq!(bytecode, "600560055f", "Expected PUSH1 0x05, PUSH1 0x05, PUSH0");
}

#[test]
fn test_large_numbers() {
    let source = r#"
        #define constant LARGE_A = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        #define constant RESULT = LARGE_A + 0x00  // Should handle max U256

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert!(bytecode.contains("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), "Expected to handle max U256 value");
}

// Integration Tests

#[test]
fn test_expression_in_code_table() {
    let source = r#"
        #define constant VALUE = 0x02 + 0x02  // 4

        #define table CODE_TABLE {
            [VALUE]
        }

        #define macro MAIN() = takes(0) returns(0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH1 0x01 (table size 1 byte), PUSH2 0x0005 (table start), then 04 (evaluated constant)
    assert_eq!(bytecode, "600161000504", "Expected code table with evaluated expression");
}

#[test]
fn test_leading_zeros_preserved_in_code_table() {
    let source = r#"
        #define constant ADDR = 0x00000001

        #define table CODE_TABLE {
            [ADDR]
        }

        #define macro MAIN() = takes(0) returns(0) {
            __tablesize(CODE_TABLE)
            __tablestart(CODE_TABLE)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    // PUSH1 0x04 (table size 4 bytes), PUSH2 0x0005 (table start), then 00000001 (with leading zeros)
    assert_eq!(bytecode, "600461000500000001", "Expected leading zeros preserved in code table");
}

#[test]
fn test_constructor_with_expressions() {
    let source = r#"
        #define constant INIT_VALUE = 0x10 + 0x05  // 21

        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            [INIT_VALUE]
            0x00 sstore
        }

        #define macro MAIN() = takes(0) returns(0) {
            0x00 sload
        }
    "#;

    let (constructor_bytecode, _) = compile_to_constructor_bytecode(source).unwrap();
    // Should contain PUSH1 0x15 (21) followed by PUSH0, SSTORE
    // Note: 0x5f is PUSH0 in newer EVM versions, 0x55 is SSTORE
    assert_eq!(constructor_bytecode, "60155f55", "Expected constructor with evaluated expression");
}

// Error Handling

#[test]
fn test_division_by_zero_error() {
    let source = r#"
        #define constant ZERO = 0x00
        #define constant RESULT = 0x10 / ZERO

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::DivisionByZero));
}

#[test]
fn test_modulo_by_zero_error() {
    let source = r#"
        #define constant RESULT = 0x10 % 0x00

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::DivisionByZero));
}

#[test]
fn test_undefined_constant_error() {
    let source = r#"
        #define constant RESULT = UNDEFINED_CONST + 0x01

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::UndefinedConstant(_)));
}

#[test]
fn test_overflow_error() {
    let source = r#"
        #define constant MAX = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        #define constant RESULT = MAX + 0x01  // Should overflow

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::ArithmeticOverflow));
}

#[test]
fn test_underflow_error() {
    let source = r#"
        #define constant RESULT = 0x01 - 0x02  // Should underflow (unsigned arithmetic)

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::ArithmeticUnderflow));
}

#[test]
fn test_multiplication_overflow() {
    let source = r#"
        #define constant LARGE = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
        #define constant RESULT = LARGE * 0x02  // Should overflow

        #define macro MAIN() = takes(0) returns(0) {
            [RESULT]
        }
    "#;

    assert_compile_error(source, |kind| matches!(kind, CodegenErrorKind::ArithmeticOverflow));
}
