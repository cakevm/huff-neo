mod common;

use crate::common::compile_to_bytecode;

// Tests for arithmetic operations combining constants and args in if/for expressions
// These test mixed expressions like [CONST] + <arg>

#[test]
fn test_combined_const_plus_arg() {
    let source = r#"
        #define constant BASE = 0x10

        #define macro TEST(offset) = takes(0) returns(0) {
            if([BASE] + <offset>) {  // 0x10 + 0x05 = 0x15 (true)
                0x42
            } else {
                0x10
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6042", "0x10 + 0x05 = 0x15 (non-zero, true)");
}

#[test]
fn test_combined_arg_plus_const() {
    let source = r#"
        #define constant OFFSET = 0x08

        #define macro TEST(base) = takes(0) returns(0) {
            if(<base> + [OFFSET]) {  // 0x20 + 0x08 = 0x28 (true)
                0xAA
            } else {
                0xBB
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x20)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60aa", "0x20 + 0x08 = 0x28 (non-zero, true)");
}

#[test]
fn test_combined_const_minus_arg() {
    let source = r#"
        #define constant TOTAL = 0x64

        #define macro TEST(amount) = takes(0) returns(0) {
            if([TOTAL] - <amount>) {  // 0x64 - 0x14 = 0x50 (true)
                0xCC
            } else {
                0xDD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x14)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60cc", "0x64 - 0x14 = 0x50 (non-zero, true)");
}

#[test]
fn test_combined_arg_minus_const() {
    let source = r#"
        #define constant REDUCTION = 0x03

        #define macro TEST(value) = takes(0) returns(0) {
            if(<value> - [REDUCTION]) {  // 0x0A - 0x03 = 0x07 (true)
                0xEE
            } else {
                0xFF
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0A)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ee", "0x0A - 0x03 = 0x07 (non-zero, true)");
}

#[test]
fn test_combined_const_multiply_arg() {
    let source = r#"
        #define constant MULTIPLIER = 0x03

        #define macro TEST(value) = takes(0) returns(0) {
            if([MULTIPLIER] * <value>) {  // 0x03 * 0x05 = 0x0F (true)
                0x11
            } else {
                0x22
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6011", "0x03 * 0x05 = 0x0F (non-zero, true)");
}

#[test]
fn test_combined_multiple_consts_and_args() {
    let source = r#"
        #define constant A = 0x10
        #define constant B = 0x02

        #define macro TEST(x, y) = takes(0) returns(0) {
            if([A] + <x> - [B] + <y>) {  // 0x10 + 0x05 - 0x02 + 0x03 = 0x16 (true)
                0x99
            } else {
                0x88
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05, 0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6099", "0x10 + 0x05 - 0x02 + 0x03 = 0x16 (non-zero, true)");
}

#[test]
fn test_combined_literal_const_arg() {
    let source = r#"
        #define constant OFFSET = 0x0A

        #define macro TEST(value) = takes(0) returns(0) {
            if(0x05 + [OFFSET] + <value>) {  // 0x05 + 0x0A + 0x03 = 0x12 (true)
                0x77
            } else {
                0x66
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6077", "0x05 + 0x0A + 0x03 = 0x12 (non-zero, true)");
}

#[test]
fn test_combined_comparison_const_equal_arg() {
    let source = r#"
        #define constant EXPECTED = 0x42

        #define macro TEST(value) = takes(0) returns(0) {
            if([EXPECTED] == <value>) {  // 0x42 == 0x42 (true)
                0xAB
            } else {
                0xCD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x42)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ab", "0x42 == 0x42 is true");
}

#[test]
fn test_combined_comparison_arg_not_equal_const() {
    let source = r#"
        #define constant ZERO = 0x00

        #define macro TEST(value) = takes(0) returns(0) {
            if(<value> != [ZERO]) {  // 0x01 != 0x00 (true)
                0x55
            } else {
                0x44
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x01)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6055", "0x01 != 0x00 is true");
}

#[test]
fn test_combined_comparison_const_less_than_arg() {
    let source = r#"
        #define constant MIN = 0x10

        #define macro TEST(value) = takes(0) returns(0) {
            if([MIN] < <value>) {  // 0x10 < 0x20 (true)
                0x33
            } else {
                0x22
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x20)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6033", "0x10 < 0x20 is true");
}

#[test]
fn test_combined_comparison_arg_greater_than_const() {
    let source = r#"
        #define constant THRESHOLD = 0x0A

        #define macro TEST(value) = takes(0) returns(0) {
            if(<value> > [THRESHOLD]) {  // 0x14 > 0x0A (true)
                0xBE
            } else {
                0xEF
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x14)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60be", "0x14 > 0x0A is true");
}

#[test]
fn test_combined_nested_expression() {
    let source = r#"
        #define constant A = 0x02
        #define constant B = 0x03

        #define macro TEST(x, y) = takes(0) returns(0) {
            if(([A] + <x>) * ([B] + <y>)) {  // (0x02 + 0x04) * (0x03 + 0x02) = 0x06 * 0x05 = 0x1E (true)
                0xCA
            } else {
                0xFE
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x04, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ca", "(0x02 + 0x04) * (0x03 + 0x02) = 0x1E (non-zero, true)");
}

#[test]
fn test_combined_precedence_with_mixed_types() {
    let source = r#"
        #define constant MULT = 0x03
        #define constant ADD = 0x02

        #define macro TEST(x) = takes(0) returns(0) {
            if([ADD] + [MULT] * <x>) {  // 0x02 + (0x03 * 0x05) = 0x02 + 0x0F = 0x11 (true)
                0xDA
            } else {
                0xDB
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x05)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60da", "0x02 + (0x03 * 0x05) = 0x11 (multiplication before addition)");
}

#[test]
fn test_combined_in_for_loop_bound() {
    let source = r#"
        #define constant BASE = 0x02

        #define macro TEST(extra) = takes(0) returns(0) {
            for(i in 0..([BASE] + <extra>)) {  // 0..4
                <i>
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "5f600160026003", "Loop from 0 to 4");
}

#[test]
fn test_combined_unary_negation_on_const_plus_arg() {
    let source = r#"
        #define constant VALUE = 0x05

        #define macro TEST(offset) = takes(0) returns(0) {
            if(-([VALUE] + <offset>)) {  // -(0x05 + 0x03) = -0x08 (non-zero, true)
                0xAC
            } else {
                0xBD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x03)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ac", "-(0x05 + 0x03) is non-zero (true)");
}

#[test]
fn test_combined_logical_not_on_mixed_comparison() {
    let source = r#"
        #define constant A = 0x10

        #define macro TEST(b) = takes(0) returns(0) {
            if(!([A] == <b>)) {  // !(0x10 == 0x20) = !0 = 1 (true)
                0xDE
            } else {
                0xAD
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x20)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60de", "!(0x10 == 0x20) is true");
}

#[test]
fn test_combined_zero_result_with_const_and_arg() {
    let source = r#"
        #define constant MINUEND = 0x0A

        #define macro TEST(subtrahend) = takes(0) returns(0) {
            if([MINUEND] - <subtrahend>) {  // 0x0A - 0x0A = 0x00 (false)
                0x11
            } else {
                0x22
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0A)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "6022", "0x0A - 0x0A = 0 (false, else branch)");
}

#[test]
fn test_combined_multiple_nested_mixed() {
    let source = r#"
        #define constant A = 0x02
        #define constant B = 0x05

        #define macro TEST(x, y) = takes(0) returns(0) {
            if(([A] * <x>) + ([B] - <y>)) {  // (0x02 * 0x03) + (0x05 - 0x02) = 0x06 + 0x03 = 0x09 (true)
                0xBA
            } else {
                0xBE
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x03, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60ba", "(0x02 * 0x03) + (0x05 - 0x02) = 0x09 (non-zero, true)");
}

#[test]
fn test_combined_const_arg_with_modulo() {
    let source = r#"
        #define constant DIVISOR = 0x05

        #define macro TEST(value) = takes(0) returns(0) {
            if(<value> % [DIVISOR]) {  // 0x0E % 0x05 = 0x04 (true)
                0x3C
            } else {
                0x3D
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x0E)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "603c", "0x0E % 0x05 = 0x04 (non-zero, true)");
}

#[test]
fn test_combined_complex_all_types() {
    let source = r#"
        #define constant C1 = 0x02
        #define constant C2 = 0x03

        #define macro TEST(a, b) = takes(0) returns(0) {
            if((0x01 + [C1]) * <a> - ([C2] + <b>)) {  // (0x01 + 0x02) * 0x04 - (0x03 + 0x02) = 0x0C - 0x05 = 0x07 (true)
                0xFA
            } else {
                0xFB
            }
        }

        #define macro MAIN() = takes(0) returns(0) {
            TEST(0x04, 0x02)
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60fa", "(0x01 + 0x02) * 0x04 - (0x03 + 0x02) = 0x07 (non-zero, true)");
}
