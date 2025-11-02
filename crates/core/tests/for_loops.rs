mod common;

use common::compile_to_bytecode;

#[test]
fn test_for_loop_basic_unrolling() {
    // Test basic for loop unrolling with literal bounds
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..3) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0, PUSH1 0x01, PUSH1 0x02
    let expected = "5f60016002";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_step() {
    // Test for loop with step clause
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..6 step 2) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0, PUSH1 0x02, PUSH1 0x04
    let expected = "5f60026004";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_constants() {
    // Test for loop with constant expressions as bounds
    let source = r#"
        #define constant START = 0x01
        #define constant END = 0x04

        #define macro MAIN() = takes(0) returns(0) {
            for(i in [START]..[END]) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x01, PUSH1 0x02, PUSH1 0x03
    let expected = "600160026003";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_arithmetic_bounds() {
    // Test for loop with arithmetic expressions in bounds
    let source = r#"
        #define constant BASE = 0x02
        #define constant MULTIPLIER = 0x03

        #define macro MAIN() = takes(0) returns(0) {
            for(i in [BASE]..[BASE]*[MULTIPLIER]) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x02, PUSH1 0x03, PUSH1 0x04, PUSH1 0x05
    // (BASE = 2, BASE * MULTIPLIER = 6, so loop is 2..6)
    let expected = "6002600360046005";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_nested() {
    // Test nested for loops
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                for(j in 0..2) {
                    <i> <j> add
                }
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode (loop unrolls to):
    // i=0, j=0: PUSH0 PUSH0 ADD
    // i=0, j=1: PUSH0 PUSH1 0x01 ADD
    // i=1, j=0: PUSH1 0x01 PUSH0 ADD
    // i=1, j=1: PUSH1 0x01 PUSH1 0x01 ADD
    let expected = "5f5f015f6001016001 5f 01600160010 1".replace(" ", "");
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_body_statements() {
    // Test for loop with multiple statements in the body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                <i>
                dup1
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0 DUP1 PUSH1 0x01 DUP1
    let expected = "5f80600180";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_sequential() {
    // Test two sequential for loops
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                <i>
            }
            for(j in 0..2) {
                <j>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0 PUSH1 0x01 PUSH0 PUSH1 0x01
    let expected = "5f60015f6001";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_edge_case_empty_range() {
    // Test for loop with empty range (start == end)
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 5..5) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: empty (no iterations)
    let expected = "";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_edge_case_single_iteration() {
    // Test for loop with single iteration
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..1) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0
    let expected = "5f";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_hex_bounds() {
    // Test for loop with hexadecimal literal bounds
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0x10..0x13) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH1 0x10, PUSH1 0x11, PUSH1 0x12
    let expected = "601060116012";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_constant_step() {
    // Test for loop with constant expression for step
    let source = r#"
        #define constant STEP_SIZE = 0x02

        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..6 step [STEP_SIZE]) {
                <i>
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: PUSH0, PUSH1 0x02, PUSH1 0x04
    let expected = "5f60026004";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_empty_body() {
    // Test for loop with empty body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..3) {
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: empty (body has no statements)
    let expected = "";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_opcodes_in_body() {
    // Test for loop with multiple opcodes in the body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                <i>
                <i>
                add
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode:
    // i=0: PUSH0 PUSH0 ADD
    // i=1: PUSH1 0x01 PUSH1 0x01 ADD
    let expected = "5f5f016001600101";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_with_label_in_body() {
    // Test for loop with label definition in body
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                loop_label:
                    <i>
                    loop_label jumpi
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode (each iteration has its own label scope):
    // i=0: JUMPDEST PUSH0 PUSH2(label_addr) JUMPI
    // i=1: JUMPDEST PUSH1 0x01 PUSH2(label_addr) JUMPI
    let expected = "5b5f610000575b600161000657";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}

#[test]
fn test_for_loop_deeply_nested() {
    // Test deeply nested for loops (3 levels)
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..2) {
                for(j in 0..2) {
                    for(k in 0..2) {
                        <i> <j> <k> add add
                    }
                }
            }
        }
    "#;

    let bytecode = compile_to_bytecode(source).unwrap();

    // Expected bytecode: 8 iterations (2^3), each with 3 pushes + 2 adds
    // Iterations: (i,j,k) = (0,0,0), (0,0,1), (0,1,0), (0,1,1), (1,0,0), (1,0,1), (1,1,0), (1,1,1)
    let expected = "5f5f5f01015f5f600101015f60015f01015f60016001010160015f5f010160015f60010101600160015f01016001600160010101";
    assert_eq!(bytecode.to_lowercase(), expected.to_lowercase());
}
