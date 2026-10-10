//! Offsets beyond 64 KiB.
//!
//! From Amsterdam, initcode may be up to 128 KiB (EIP-7954), so a constructor can be larger than
//! 64 KiB and its jumps may need PUSH3 targets. Runtime code stays within 64 KiB, so its targets
//! always fit into PUSH2. Fields with a fixed 2-byte format must report an error instead of
//! producing corrupt bytecode.

use alloy_primitives::hex;
use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;
use std::sync::Arc;

fn amsterdam() -> EVMVersion {
    EVMVersion::new(SupportedEVMVersions::Amsterdam)
}

fn parse(source: &str) -> Contract {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();
    contract
}

/// Compiles the CONSTRUCTOR macro body for Amsterdam.
fn compile_constructor(source: &str, relax_jumps: bool) -> String {
    let (bytecode, _) = Codegen::generate_constructor_bytecode(&amsterdam(), &parse(source), None, relax_jumps).unwrap();
    bytecode
}

/// Compiles the MAIN macro for the default EVM version.
fn compile_main(source: &str, relax_jumps: bool) -> String {
    Codegen::generate_main_bytecode(&EVMVersion::default(), &parse(source), None, relax_jumps).unwrap()
}

/// Assembles the full deployment bytecode for Amsterdam.
fn compile_deployment(source: &str) -> Result<Artifact, CodegenError> {
    let mut contract = parse(source);
    let evm = &amsterdam();
    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, false)?;
    contract.runtime_size = Some(main.bytecode.len() / 2);
    let (constructor, updated_contract) = Codegen::generate_constructor_macro_bytecode(evm, &contract, None, false)?;
    Codegen::new().assemble_artifact(evm, Arc::new(FileSource::default()), &updated_contract, vec![], main, Some(constructor), false)
}

/// Walks the bytecode and checks that every PUSH followed by JUMP or JUMPI targets a JUMPDEST.
///
/// Returns the push width and target of each jump in order.
fn checked_jumps(hex_code: &str) -> Vec<(usize, usize)> {
    let code = hex::decode(hex_code).unwrap();
    let mut jumps = vec![];
    let mut pc = 0;
    while pc < code.len() {
        let op = code[pc];
        if (0x60..=0x7f).contains(&op) {
            let width = (op - 0x5f) as usize;
            let next = pc + 1 + width;
            if next < code.len() && matches!(code[next], 0x56 | 0x57) {
                let target = code[pc + 1..next].iter().fold(0usize, |acc, b| (acc << 8) | *b as usize);
                assert_eq!(code.get(target), Some(&0x5b), "jump at {pc:#x} targets {target:#x}, which is not a JUMPDEST");
                jumps.push((width, target));
            }
            pc = next;
        } else {
            pc += 1;
        }
    }
    jumps
}

/// Widths of the checked jumps, in order.
fn jump_widths(hex_code: &str) -> Vec<usize> {
    checked_jumps(hex_code).into_iter().map(|(width, _)| width).collect()
}

#[test]
fn test_forward_jump_beyond_64k_uses_push3() {
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            far jump
            for(i in 0..65536) { stop }
            far:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    let bytecode = compile_constructor(source, false);

    // PUSH3 + JUMP is 5 bytes, so the label sits at 5 + 65536 = 0x10005
    assert!(bytecode.starts_with("6201000556"), "unexpected start: {}", &bytecode[..12]);
    assert_eq!(checked_jumps(&bytecode), vec![(3, 0x10005)]);
}

#[test]
fn test_jump_target_at_0xffff_stays_push2() {
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            edge jump
            for(i in 0..65531) { stop }
            edge:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    assert_eq!(checked_jumps(&compile_constructor(source, false)), vec![(2, 0xffff)]);
}

#[test]
fn test_widening_cascades_to_jumps_pushed_over_the_boundary() {
    // `near` starts at 0xffff and fits PUSH2, but widening the jump to `far` moves it to 0x10000,
    // so the jump to `near` has to widen in a second round
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            near jump
            far jump
            for(i in 0..65527) { stop }
            near:
            for(i in 0..10) { stop }
            far:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    assert_eq!(checked_jumps(&compile_constructor(source, false)), vec![(3, 0x10001), (3, 0x1000c)]);
}

#[test]
fn test_backward_jumps_beyond_64k() {
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            start:
            for(i in 0..65536) { stop }
            back:
            start jump
            back jump
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    assert_eq!(checked_jumps(&compile_constructor(source, false)), vec![(2, 0x0), (3, 0x10001)]);
}

#[test]
fn test_labels_in_nested_macros_beyond_64k() {
    let source = r#"
        #define macro INNER() = takes(0) returns(0) {
            inner jump
            inner:
        }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            for(i in 0..65536) { stop }
            INNER()
            after jumpi
            after:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    assert_eq!(jump_widths(&compile_constructor(source, false)), vec![3, 3]);
}

#[test]
fn test_relaxed_and_widened_jumps_combined() {
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            near jump
            near:
            far jump
            for(i in 0..65536) { stop }
            far:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    // Without relaxation both start as PUSH2 and only the far jump widens
    assert_eq!(jump_widths(&compile_constructor(source, false)), vec![2, 3]);
    // With relaxation the near jump shrinks to PUSH1 while the far jump stays PUSH3
    assert_eq!(jump_widths(&compile_constructor(source, true)), vec![1, 3]);
}

#[test]
fn test_self_referencing_codesize_beyond_64k() {
    let source = r#"
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            __codesize(CONSTRUCTOR) pop
            target jump
            for(i in 0..65530) { stop }
            target:
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    let bytecode = compile_constructor(source, false);
    let length = bytecode.len() / 2;
    assert!(length > 0xffff);

    // The macro's own size needs PUSH3 and must count the widened pushes
    assert_eq!(&bytecode[..8], format!("62{length:06x}"));
    assert_eq!(jump_widths(&bytecode), vec![3]);
}

#[test]
fn test_self_referencing_codesize_growth_keeps_jumps_valid() {
    // The size grows from PUSH1 to PUSH2 before the label, which must move the jump target
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            __codesize(MAIN) pop
            target jump
            for(i in 0..300) { stop }
            target:
                stop
        }
    "#;

    let bytecode = compile_main(source, false);
    assert_eq!(&bytecode[..6], format!("61{:04x}", bytecode.len() / 2));
    assert_eq!(jump_widths(&bytecode), vec![2]);
}

#[test]
fn test_nested_self_referencing_codesize_beyond_64k() {
    // X is placed beyond 64 KiB, so its jump widens to PUSH3 after X itself was compiled
    let source = r#"
        #define macro X() = takes(0) returns(0) {
            __codesize(X) pop
            target jump
            target:
        }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            for(i in 0..65536) { stop }
            X()
            stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    for relax_jumps in [false, true] {
        let bytecode = compile_constructor(source, relax_jumps);
        let x = &bytecode[65536 * 2..bytecode.len() - 2];
        // PUSH1 size, POP, PUSH3 target, JUMP, JUMPDEST
        assert_eq!(x.len() / 2, 9);
        assert_eq!(&x[..4], "6009", "__codesize(X) must count the widened jump");
        assert_eq!(jump_widths(&bytecode), vec![3]);
    }
}

#[test]
fn test_nested_self_referencing_codesize_with_relaxed_jumps() {
    // X's jump only shrinks to PUSH1 when the invoking macro is resolved
    let source = r#"
        #define macro X() = takes(0) returns(0) {
            __codesize(X) pop
            target jump
            target:
        }
        #define macro MAIN() = takes(0) returns(0) {
            0x00 pop
            X()
            stop
        }
    "#;

    // PUSH0, POP, then X: PUSH1 size, POP, PUSH2/PUSH1 target, JUMP, JUMPDEST, then STOP
    assert_eq!(compile_main(source, false), "5f50600850610009565b00");
    assert_eq!(compile_main(source, true), "5f506007506008565b00");
}

#[test]
fn test_codesize_of_enclosing_macro_from_nested_macro() {
    // `__codesize` of a macro that is being compiled measures that macro's invocation, not the
    // outermost macro
    let source = r#"
        #define macro INNER() = takes(0) returns(0) {
            __codesize(INNER) __codesize(OUTER)
        }
        #define macro OUTER() = takes(0) returns(0) {
            pc INNER() pc
        }
        #define macro MAIN() = takes(0) returns(0) {
            0x00 pop
            OUTER()
            __codesize(MAIN)
        }
    "#;

    let bytecode = compile_main(source, false);
    // PUSH0, POP, PC, PUSH1 4 (INNER), PUSH1 6 (OUTER), PC, PUSH1 10 (MAIN)
    assert_eq!(bytecode, "5f50586004600658600a");
}

#[test]
fn test_max_size_runtime_with_table() {
    // A 64 KiB runtime with a jump and a table at its very end: all offsets fit into PUSH2
    let source = r#"
        #define table T { 0xaabbccdd }
        #define macro MAIN() = takes(0) returns(0) {
            __tablestart(T) pop
            target jump
            for(i in 0..65523) { stop }
            target:
        }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {}
    "#;

    let artifact = compile_deployment(source).unwrap();
    let runtime = artifact.runtime;
    assert_eq!(runtime.len() / 2, EIP7954_MAX_CODE_SIZE);
    assert!(runtime.starts_with("61fffc50"), "__tablestart must point at 0xfffc: {}", &runtime[..8]);
    assert!(runtime.ends_with("5baabbccdd"));
    assert_eq!(checked_jumps(&runtime), vec![(2, 0xfffb)]);
}

#[test]
fn test_tablestart_after_relaxed_jump() {
    // `__tablestart` is filled by position, which must follow jumps that shrank before it
    let source = r#"
        #define table T { 0xaabb }
        #define macro MAIN() = takes(0) returns(0) {
            lbl jump
            lbl:
            __tablestart(T)
            __tablesize(T)
        }
    "#;

    let bytecode = compile_main(source, true);
    // PUSH1 0x03, JUMP, JUMPDEST, PUSH2 0x0009, PUSH1 0x02, table
    assert_eq!(bytecode, "6003565b6100096002aabb");
}

#[test]
fn test_packed_jump_table_beyond_64k_is_rejected() {
    let source = r#"
        #define jumptable__packed PACKED { far }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            __tablesize(PACKED) pop
            for(i in 0..65536) { stop }
            far:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    let error = Codegen::generate_constructor_bytecode(&amsterdam(), &parse(source), None, false).unwrap_err();
    assert!(
        matches!(&error.kind, CodegenErrorKind::OffsetExceedsTwoBytes(what, offset) if what.contains("far") && *offset > 0xffff),
        "unexpected error {:?}",
        error.kind
    );
}

#[test]
fn test_regular_jump_table_beyond_64k() {
    // Regular jump tables store 32-byte entries, so any offset fits
    let source = r#"
        #define jumptable TABLE { far }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            __tablesize(TABLE) pop
            for(i in 0..65536) { stop }
            far:
                stop
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    let bytecode = compile_constructor(source, false);
    // PUSH1 0x20, POP, 65536 STOPs, JUMPDEST at 0x10003, STOP, then the 32-byte entry
    assert!(bytecode.ends_with(&format!("5b00{:064x}", 0x10003)));
}

#[test]
fn test_constructor_tablestart_beyond_64k_is_rejected() {
    // Constructor-only tables are placed after the runtime, which here starts beyond 64 KiB
    let source = r#"
        #define table T { 0xaabb }
        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            __tablestart(T) pop
            for(i in 0..65536) { stop }
        }
        #define macro MAIN() = takes(0) returns(0) { stop }
    "#;

    let error = compile_deployment(source).unwrap_err();
    assert!(
        matches!(&error.kind, CodegenErrorKind::OffsetExceedsTwoBytes(what, offset) if what.contains("\"T\"") && *offset > 0xffff),
        "unexpected error {:?}",
        error.kind
    );
}

#[test]
fn test_dynamic_constructor_argument_beyond_64k_is_rejected() {
    let main = Codegen::generate_main_bytecode(
        &amsterdam(),
        &parse("#define macro MAIN() = { __CODECOPY_DYN_ARG(0x00, 0x20) } #define macro CONSTRUCTOR() = {}"),
        None,
        false,
    )
    .unwrap();
    let args = Codegen::encode_constructor_args(vec![String::from("testing")]);

    // The arguments are appended after a constructor of more than 64 KiB
    let constructor = "00".repeat(0x10000);
    let error = Codegen::new()
        .churn(&amsterdam(), Arc::new(FileSource::default()), args, &main, &constructor, false, None, None, false)
        .unwrap_err();
    assert!(
        matches!(&error.kind, CodegenErrorKind::OffsetExceedsTwoBytes(what, offset) if what.contains("__CODECOPY_DYN_ARG") && *offset > 0xffff),
        "unexpected error {:?}",
        error.kind
    );
}
