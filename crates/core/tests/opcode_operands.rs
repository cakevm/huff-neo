mod common;

use common::{assert_compile_error, compile_to_bytecode};
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

/// Parses a source and validates its opcodes against the given EVM version.
fn validate_for(source: &str, version: SupportedEVMVersions) -> Result<(), CodegenError> {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    contract.validate_opcodes(&EVMVersion::new(version))
}

#[test]
fn test_stack_operands_from_constants() {
    let source = r#"
        #define constant DEPTH = 0x12
        #define constant N = 0x01
        #define constant M = 0x02

        #define macro MAIN() = takes(0) returns(0) {
            dupn [DEPTH]
            swapn [DEPTH]
            exchange [N] [M]
        }
    "#;

    // DUPN 18 = e6 81, SWAPN 18 = e7 81, EXCHANGE 1 2 = e8 8e
    assert_eq!(compile_to_bytecode(source).unwrap(), "e681e781e88e");
}

#[test]
fn test_stack_operands_from_arithmetic() {
    let source = r#"
        #define constant BASE = 0x10

        #define macro MAIN() = takes(0) returns(0) {
            dupn ([BASE] + 1)
            swapn ([BASE] * 2)
            exchange ([BASE] - 15) ([BASE] / 8)
        }
    "#;

    // DUPN 17 = e6 80, SWAPN 32 = e7 8f, EXCHANGE 1 2 = e8 8e
    assert_eq!(compile_to_bytecode(source).unwrap(), "e680e78fe88e");
}

#[test]
fn test_stack_operands_from_macro_args() {
    let source = r#"
        #define constant DEPTH = 0x14

        #define macro DUP_AT(depth) = takes(0) returns(0) {
            dupn <depth>
        }

        #define macro SWAP_PAIR(n, m) = takes(0) returns(0) {
            exchange <n> <m>
        }

        #define macro MAIN() = takes(0) returns(0) {
            DUP_AT(0x11)    // hex literal
            DUP_AT(18)      // decimal literal
            DUP_AT(DEPTH)   // constant
            SWAP_PAIR(2, 5)
        }
    "#;

    // DUPN 17 = e6 80, DUPN 18 = e6 81, DUPN 20 = e6 83, EXCHANGE 2 5 = e8 9b
    assert_eq!(compile_to_bytecode(source).unwrap(), "e680e681e683e89b");
}

#[test]
fn test_stack_operands_from_nested_macro_args() {
    let source = r#"
        #define macro INNER(depth) = takes(0) returns(0) {
            swapn <depth>
        }

        #define macro OUTER(depth) = takes(0) returns(0) {
            INNER(<depth>)
        }

        #define macro MAIN() = takes(0) returns(0) {
            OUTER(0x11)
        }
    "#;

    // SWAPN 17 = e7 80
    assert_eq!(compile_to_bytecode(source).unwrap(), "e780");
}

#[test]
fn test_stack_operands_from_loop_variables() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            for(i in 17..19) {
                dupn <i>
            }
            for(i in 0..2) {
                swapn (<i> + 17)
                exchange 1 (<i> + 2)
            }
        }
    "#;

    // DUPN 17, DUPN 18, then SWAPN 17, EXCHANGE 1 2, SWAPN 18, EXCHANGE 1 3
    assert_eq!(compile_to_bytecode(source).unwrap(), "e680e681e780e88ee781e88d");
}

#[test]
fn test_stack_operands_in_labels_and_conditionals() {
    let source = r#"
        #define constant DEEP = 0x01
        #define constant DEPTH = 0x11

        #define macro MAIN() = takes(0) returns(0) {
            start:
                dupn [DEPTH]
            if ([DEEP] == 0x01) {
                swapn [DEPTH]
            } else {
                swap1
            }
        }
    "#;

    // JUMPDEST, DUPN 17, SWAPN 17
    assert_eq!(compile_to_bytecode(source).unwrap(), "5be680e780");
}

#[test]
fn test_resolved_operands_count_towards_label_offsets() {
    let source = r#"
        #define constant DEPTH = 0x11

        #define macro MAIN() = takes(0) returns(0) {
            dupn [DEPTH]
            push2 [DEPTH]
            target jump
            target:
                stop
        }
    "#;

    // e6 80 | PUSH2 0x0011 | PUSH2 0x0009 | JUMP | JUMPDEST | STOP
    assert_eq!(compile_to_bytecode(source).unwrap(), "e680610011610009565b00");
}

#[test]
fn test_push_operands_are_padded_to_push_width() {
    let source = r#"
        #define constant SIZE = 0x12
        #define constant WORD = 0x0102

        #define macro PUSH_ARG(value) = takes(0) returns(0) {
            push4 <value>
        }

        #define macro MAIN() = takes(0) returns(0) {
            push2 [SIZE]
            push32 [WORD]
            push1 ([SIZE] + 1)
            push1 18
            PUSH_ARG(0xff)
            for(i in 0..2) {
                push2 <i>
            }
        }
    "#;

    let expected = [
        "610012",                                                             // push2 [SIZE]
        "7f0000000000000000000000000000000000000000000000000000000000000102", // push32 [WORD]
        "6013",                                                               // push1 ([SIZE] + 1)
        "6012",                                                               // push1 18
        "63000000ff",                                                         // push4 <value>
        "610000",                                                             // push2 <i>, i = 0
        "610001",                                                             // push2 <i>, i = 1
    ]
    .concat();
    assert_eq!(compile_to_bytecode(source).unwrap(), expected);
}

#[test]
fn test_resolved_stack_operand_out_of_range() {
    for operand in ["[SHALLOW]", "<depth>", "([SHALLOW] - 1)"] {
        let source = format!(
            r#"
            #define constant SHALLOW = 0x10

            #define macro DUP_AT(depth) = takes(0) returns(0) {{
                dupn {operand}
            }}

            #define macro MAIN() = takes(0) returns(0) {{
                DUP_AT(0x10)
            }}
        "#
        );
        assert_compile_error(&source, |k| matches!(k, CodegenErrorKind::InvalidOpcodeOperand(_)));
    }
}

#[test]
fn test_resolved_exchange_operands_out_of_range() {
    let source = r#"
        #define constant N = 0x02

        #define macro MAIN() = takes(0) returns(0) {
            exchange [N] [N]
        }
    "#;
    assert_compile_error(source, |k| matches!(k, CodegenErrorKind::InvalidOpcodeOperand(_)));
}

#[test]
fn test_resolved_push_operand_overflow() {
    let source = r#"
        #define constant BIG = 0x1234

        #define macro MAIN() = takes(0) returns(0) {
            push1 [BIG]
        }
    "#;
    assert_compile_error(source, |k| matches!(k, CodegenErrorKind::InvalidOpcodeOperand(msg) if msg.contains("0x1234")));
}

#[test]
fn test_operand_from_label_argument_is_rejected() {
    let source = r#"
        #define macro DUP_AT(depth) = takes(0) returns(0) {
            dupn <depth>
        }

        #define macro MAIN() = takes(0) returns(0) {
            DUP_AT(some_label)
            some_label:
        }
    "#;
    assert_compile_error(source, |k| matches!(k, CodegenErrorKind::InvalidMacroArgumentType(_)));
}

#[test]
fn test_operand_from_undefined_constant_is_rejected() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            dupn [MISSING]
        }
    "#;
    assert!(compile_to_bytecode(source).is_err());
}

#[test]
fn test_resolved_operands_require_amsterdam() {
    let source = r#"
        #define constant DEPTH = 0x11

        #define macro MAIN() = takes(0) returns(0) {
            for(i in 0..1) {
                dupn [DEPTH]
            }
        }
    "#;

    assert!(validate_for(source, SupportedEVMVersions::Amsterdam).is_ok());
    let error = validate_for(source, SupportedEVMVersions::Osaka).unwrap_err();
    assert!(
        matches!(error.kind, CodegenErrorKind::InvalidOpcodeForEVMVersion(ref opcode, ref required, _) if opcode == "dupn" && required == "amsterdam"),
        "unexpected error {:?}",
        error.kind
    );
}

#[test]
fn test_operand_beyond_machine_word_is_rejected() {
    // Values that do not even fit into a usize must be reported, not truncated
    let source = r#"
        #define constant HUGE = 0x010000000000000011

        #define macro MAIN() = takes(0) returns(0) {
            dupn [HUGE]
        }
    "#;
    assert_compile_error(source, |k| matches!(k, CodegenErrorKind::InvalidOpcodeOperand(_)));
}
