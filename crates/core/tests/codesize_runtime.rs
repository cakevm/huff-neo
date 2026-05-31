//! Tests for the `__codesize(RUNTIME)` builtin form.
//!
//! `__codesize(RUNTIME)` resolves at compile time to the byte length of the runtime section
//! (MAIN body + appended runtime tables). Usable in both the constructor and MAIN; MAIN
//! codegen iterates to a fixed point on the self-referential size.

mod common;

use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn runtime_codesize_inline_in_constructor() {
    // MAIN compiles to 5 bytes (`PUSH1 0x01 PUSH1 0x02 ADD` = `60016002 01`).
    // The constructor returns its own bytecode (custom bootstrap), so `__codesize(RUNTIME)` is
    // exposed and resolves to 5. The constant has been padded to PUSH1 by the codegen.
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) { 0x01 0x02 add }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            __codesize(RUNTIME) pop 0x00 0x00 return
        }
    "#;

    let bytecode = common::compile_to_deployment(source);
    assert_eq!(bytecode, "6005505f5ff36001600201");
}

#[test]
fn runtime_codesize_via_constant_wrapper() {
    let source = r#"
        #define constant SIZE = __codesize(RUNTIME)

        #define macro MAIN() = takes(0) returns(0) { 0x01 0x02 add }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            [SIZE] pop 0x00 0x00 return
        }
    "#;

    let bytecode = common::compile_to_deployment(source);
    assert_eq!(bytecode, "6005505f5ff36001600201");
}

#[test]
fn runtime_codesize_in_arithmetic_expression() {
    // 5 + 0x20 = 0x25 (37). Verifies the value flows through constant arithmetic.
    let source = r#"
        #define constant SIZE = __codesize(RUNTIME)
        #define constant SIZE_PLUS = [SIZE] + 0x20

        #define macro MAIN() = takes(0) returns(0) { 0x01 0x02 add }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            [SIZE_PLUS] pop 0x00 0x00 return
        }
    "#;

    let bytecode = common::compile_to_deployment(source);
    assert!(bytecode.starts_with("6025"), "expected PUSH1 0x25 prefix, got {bytecode}");
}

#[test]
fn runtime_codesize_inline_in_main() {
    // MAIN embeds its own runtime size. Iteration converges: 0 → 2 (PUSH1 + value).
    // Trial bytecodes: pass 1 with runtime_size=0 emits `60 00 50` (PUSH1 0x00 POP) = 3 bytes.
    // Pass 2 with runtime_size=3 emits `60 03 50` (PUSH1 0x03 POP) = 3 bytes. Stable.
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) { __codesize(RUNTIME) pop }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "60 03 50".replace(' ', ""));
}

#[test]
fn runtime_codesize_via_constant_in_main() {
    let source = r#"
        #define constant SIZE = __codesize(RUNTIME)
        #define macro MAIN() = takes(0) returns(0) { [SIZE] pop }
    "#;

    let bytecode = common::compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "600350");
}

#[test]
fn runtime_codesize_arithmetic_in_main() {
    let source = r#"
        #define constant SIZE = __codesize(RUNTIME)
        #define constant SIZE_PLUS = [SIZE] + 0x20
        #define macro MAIN() = takes(0) returns(0) { [SIZE_PLUS] pop }
    "#;

    // MAIN = PUSH1 (0x03 + 0x20) POP = `60 23 50`.
    let bytecode = common::compile_to_bytecode(source).unwrap();
    assert_eq!(bytecode, "602350");
}

#[test]
fn runtime_codesize_used_in_both_main_and_constructor() {
    // Same constant referenced from both MAIN and CONSTRUCTOR — both PUSHes embed the same
    // converged runtime size (= MAIN's own length).
    let source = r#"
        #define constant SIZE = __codesize(RUNTIME)

        #define macro MAIN() = takes(0) returns(0) { [SIZE] pop }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            [SIZE] pop 0x00 0x00 return
        }
    "#;

    let bytecode = common::compile_to_deployment(source);
    // Constructor: 60 03 50 5f 5f f3 (PUSH1 0x03 POP PUSH0 PUSH0 RETURN, custom bootstrap)
    // Main:        60 03 50           (PUSH1 0x03 POP)
    assert_eq!(bytecode, "6003505f5ff3600350");
}

#[test]
fn runtime_codesize_full_immutables_example() {
    // The program from issue #166: MAIN reads an immutable at a compile-time-resolved offset.
    let source = r#"
        #define constant IMMUTABLE_1 = __codesize(RUNTIME)
        #define constant IMMUTABLE_2 = [IMMUTABLE_1] + 0x20
        #define constant RETURN_LEN  = [IMMUTABLE_2] + 0x20

        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            [IMMUTABLE_1] __codesize(CONSTRUCTOR) 0x00 codecopy
            0x11 [IMMUTABLE_1] mstore
            0x22 [IMMUTABLE_2] mstore
            [RETURN_LEN] 0x00 return
        }

        #define macro MAIN() = takes(0) returns(0) {
            0x20 [IMMUTABLE_2] 0x00 codecopy
            0x20 0x00 return
        }
    "#;

    // MAIN body: PUSH1 0x20, PUSH1 <IMMUTABLE_2>, PUSH0, CODECOPY, PUSH1 0x20, PUSH0, RETURN
    // = 10 bytes (PUSH0 used for 0x00 under Shanghai+). IMMUTABLE_2 resolves to
    // main_size + 0x20 = 10 + 0x20 = 0x2a. The exact bytecode pins both the convergence and
    // the literal embedding (no CODESIZE opcode).
    let main = common::compile_to_bytecode(source).unwrap();
    assert_eq!(main, "6020602a5f3960205ff3");
}

#[test]
fn runtime_codesize_convergence_with_push2_growth() {
    // Build a MAIN whose runtime size crosses 256 bytes, forcing the embedded PUSH to grow
    // from PUSH1 to PUSH2 between iterations. Convergence handles it.
    let pad: String = "00".repeat(260); // 260 bytes of zero bytes embedded as raw hex.
    let source = format!(
        r#"
        #define macro MAIN() = takes(0) returns(0) {{
            __codesize(RUNTIME) pop
            __VERBATIM(0x{pad})
        }}
    "#
    );

    let bytecode = common::compile_to_bytecode(&source).unwrap();
    // Expected: PUSH2 <size> POP <260 zero bytes>. PUSH2 + value (3 bytes) + POP (1) + verbatim (260) = 264 = 0x108.
    assert!(bytecode.starts_with("610108"), "expected PUSH2 0x0108 prefix, got prefix {}", &bytecode[..8]);
}

#[test]
fn runtime_codesize_in_runtime_code_table_errors() {
    // Code tables can't depend on the runtime size — that would create a circular dependency
    // in table sizing.
    let source = r#"
        #define table T { __codesize(RUNTIME) }
        #define macro MAIN() = takes(0) returns(0) { __tablestart(T) pop }
    "#;

    common::assert_compile_error(source, |k| matches!(k, CodegenErrorKind::CodesizeRuntimeInCodeTable));
}

#[test]
fn runtime_is_reserved_macro_name() {
    let source = r#"
        #define macro RUNTIME() = takes(0) returns(0) { 0x00 }
        #define macro MAIN() = takes(0) returns(0) { RUNTIME() }
    "#;

    let full_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let err = parser.parse().expect_err("expected parser to reject RUNTIME as a macro name");
    assert!(matches!(err.kind, ParserErrorKind::InvalidMacroName), "got: {err:?}");
}
