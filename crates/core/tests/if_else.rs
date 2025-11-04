mod common;

use common::compile_to_bytecode;

#[test]
fn test_if_condition_true() {
    // Test if statement with true condition (non-zero constant)
    let source = r#"
        #define constant ENABLED = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([ENABLED]) {
                0x42
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x42 (then branch is compiled)
    let expected = "6042";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_condition_false() {
    // Test if statement with false condition (zero constant)
    let source = r#"
        #define constant DISABLED = 0x00

        #define macro MAIN() = takes(0) returns(0) {
            if ([DISABLED]) {
                0x42
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: empty (then branch is not compiled)
    let expected = "";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_else_true_condition() {
    // Test if/else with true condition
    let source = r#"
        #define constant MODE = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([MODE]) {
                0xAA
            } else {
                0xBB
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xAA (then branch only)
    let expected = "60aa";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_else_false_condition() {
    // Test if/else with false condition
    let source = r#"
        #define constant MODE = 0x00

        #define macro MAIN() = takes(0) returns(0) {
            if ([MODE]) {
                0xAA
            } else {
                0xBB
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xBB (else branch only)
    let expected = "60bb";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_else_if_first_match() {
    // Test if/else if chain where first condition matches
    let source = r#"
        #define constant VALUE = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([VALUE] == 0x01) {
                0x11
            } else if ([VALUE] == 0x02) {
                0x22
            } else {
                0x33
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x11 (first condition is true)
    let expected = "6011";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_else_if_second_match() {
    // Test if/else if chain where second condition matches
    let source = r#"
        #define constant VALUE = 0x02

        #define macro MAIN() = takes(0) returns(0) {
            if ([VALUE] == 0x01) {
                0x11
            } else if ([VALUE] == 0x02) {
                0x22
            } else {
                0x33
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x22 (second condition is true)
    let expected = "6022";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_else_if_else_fallthrough() {
    // Test if/else if/else where else is taken
    let source = r#"
        #define constant VALUE = 0x03

        #define macro MAIN() = takes(0) returns(0) {
            if ([VALUE] == 0x01) {
                0x11
            } else if ([VALUE] == 0x02) {
                0x22
            } else {
                0x33
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x33 (else branch is taken)
    let expected = "6033";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_comparison_equal() {
    // Test if with == comparison operator
    let source = r#"
        #define constant A = 0x05
        #define constant B = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] == [B]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (5 == 5 is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_comparison_not_equal() {
    // Test if with != comparison operator
    let source = r#"
        #define constant A = 0x05
        #define constant B = 0x06

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] != [B]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (5 != 6 is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_comparison_less_than() {
    // Test if with < comparison operator
    let source = r#"
        #define constant A = 0x05
        #define constant B = 0x0A

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] < [B]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (5 < 10 is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_comparison_greater_than() {
    // Test if with > comparison operator
    let source = r#"
        #define constant A = 0x0A
        #define constant B = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] > [B]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (10 > 5 is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_comparison_less_equal() {
    // Test if with <= comparison operator
    let source = r#"
        #define constant A = 0x05
        #define constant B = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] <= [B]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (5 <= 5 is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_comparison_greater_equal() {
    // Test if with >= comparison operator
    let source = r#"
        #define constant A = 0x05
        #define constant B = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] >= [B]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (5 >= 5 is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_logical_not_true() {
    // Test if with logical NOT operator (true case)
    let source = r#"
        #define constant FLAG = 0x00

        #define macro MAIN() = takes(0) returns(0) {
            if (![FLAG]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (!0 = 1, which is true)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_logical_not_false() {
    // Test if with logical NOT operator (false case)
    let source = r#"
        #define constant FLAG = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if (![FLAG]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: empty (!1 = 0, which is false)
    let expected = "";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_arithmetic_in_condition() {
    // Test if with arithmetic expression in condition
    let source = r#"
        #define constant A = 0x10
        #define constant B = 0x20
        #define constant THRESHOLD = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if ([A] + [B] > [THRESHOLD]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (0x10 + 0x20 = 0x30, which is > 0x05)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_nested() {
    // Test nested if statements
    let source = r#"
        #define constant OUTER = 0x01
        #define constant INNER = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([OUTER]) {
                if ([INNER]) {
                    0xAA
                } else {
                    0xBB
                }
            } else {
                0xCC
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xAA (both conditions are true)
    let expected = "60aa";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_nested_outer_false() {
    // Test nested if with outer condition false
    let source = r#"
        #define constant OUTER = 0x00
        #define constant INNER = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([OUTER]) {
                if ([INNER]) {
                    0xAA
                } else {
                    0xBB
                }
            } else {
                0xCC
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xCC (outer condition is false)
    let expected = "60cc";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_nested_inner_false() {
    // Test nested if with inner condition false
    let source = r#"
        #define constant OUTER = 0x01
        #define constant INNER = 0x00

        #define macro MAIN() = takes(0) returns(0) {
            if ([OUTER]) {
                if ([INNER]) {
                    0xAA
                } else {
                    0xBB
                }
            } else {
                0xCC
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xBB (outer true, inner false)
    let expected = "60bb";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_with_multiple_statements() {
    // Test if with multiple statements in body
    let source = r#"
        #define constant FLAG = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([FLAG]) {
                0x42
                0x43
                add
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x42 PUSH1 0x43 ADD
    let expected = "604260 4301";
    assert_eq!(bytecode.to_lowercase(), expected.replace(" ", "").to_lowercase());
}

#[test]
fn test_if_sequential() {
    // Test two sequential if statements
    let source = r#"
        #define constant FLAG_A = 0x01
        #define constant FLAG_B = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([FLAG_A]) {
                0xAA
            }
            if ([FLAG_B]) {
                0xBB
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xAA PUSH1 0xBB
    let expected = "60aa60bb";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_empty_body() {
    // Test if with empty body (should compile to nothing)
    let source = r#"
        #define constant FLAG = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([FLAG]) {
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: empty (body has no statements)
    let expected = "";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_complex_condition() {
    // Test if with complex nested arithmetic and comparison
    let source = r#"
        #define constant A = 0x02
        #define constant B = 0x03
        #define constant C = 0x04
        #define constant D = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if (([A] + [B]) * [C] > [D]) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF ((2 + 3) * 4 = 20, which is > 5)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_literal_condition() {
    // Test if with literal value as condition
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (0x01) {
                0xAA
            } else {
                0xBB
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xAA (0x01 is non-zero, thus true)
    let expected = "60aa";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_literal_zero_condition() {
    // Test if with literal zero as condition
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            if (0x00) {
                0xAA
            } else {
                0xBB
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xBB (0x00 is zero, thus false)
    let expected = "60bb";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_multiple_else_if() {
    // Test if with multiple else if clauses
    let source = r#"
        #define constant VALUE = 0x03

        #define macro MAIN() = takes(0) returns(0) {
            if ([VALUE] == 0x01) {
                0x11
            } else if ([VALUE] == 0x02) {
                0x22
            } else if ([VALUE] == 0x03) {
                0x33
            } else if ([VALUE] == 0x04) {
                0x44
            } else {
                0x55
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x33 (third condition matches)
    let expected = "6033";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_with_opcodes_in_body() {
    // Test if with multiple opcodes in body
    let source = r#"
        #define constant FLAG = 0x01

        #define macro MAIN() = takes(0) returns(0) {
            if ([FLAG]) {
                0x00 mstore
                0x20 0x00 return
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0 MSTORE PUSH1 0x20 PUSH0 RETURN
    let expected = "5f5260205ff3";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_if_with_constant_arithmetic() {
    // Test if with constant arithmetic in condition
    let source = r#"
        #define constant BASE = 0x0A
        #define constant OFFSET = 0x05

        #define macro MAIN() = takes(0) returns(0) {
            if ([BASE] - [OFFSET] == 0x05) {
                0xFF
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0xFF (10 - 5 = 5, which equals 5)
    let expected = "60ff";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}
