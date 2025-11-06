use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

/// Tests for ArgCall resolution within nested macro invocations
///
/// These tests verify that argument calls (`<arg>`) within macro call arguments
/// are properly resolved by traversing the scope chain to find their definitions.
///
/// Example pattern: ADD(GET(<ddd>), 0x02) where <ddd> is defined in a parent scope

#[test]
fn test_simple_argcall_in_nested_macrocall() {
    // Basic case: ArgCall directly within MacroCall argument
    // Pattern: ParentMacro(ChildMacro(<arg>))
    let source = r#"
        #define macro GET(aaa) = takes(0) returns(1) {
            <aaa>
        }

        #define macro RUN(ddd) = takes(0) returns(1) {
            GET(<ddd>)
        }

        #define macro MAIN() = takes(0) returns(1) {
            RUN(0x01)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x01
    assert_eq!(main_bytecode.to_lowercase(), "6001");
}

#[test]
fn test_argcall_in_macrocall_with_multiple_args() {
    // Core case: ArgCall within MacroCall as one of multiple arguments
    // Pattern: ParentMacro(ChildMacro(<arg>), literal)
    let source = r#"
        #define macro GET(aaa) = takes(0) returns(1) {
            <aaa>
        }

        #define macro ADD(bbb, ccc) = takes(2) returns(1) {
            <bbb> <ccc> add
        }

        #define macro RUN(ddd) = takes(0) returns(1) {
            ADD(GET(<ddd>), 0x02)
        }

        #define macro MAIN() = takes(0) returns(1) {
            RUN(0x01)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x01, PUSH1 0x02, ADD
    assert_eq!(main_bytecode.to_lowercase(), "6001600201");
}

#[test]
fn test_multiple_argcalls_in_macrocall() {
    // Advanced case: Multiple ArgCalls within same MacroCall
    // Pattern: ParentMacro(ChildMacro(<arg1>), <arg2>)
    let source = r#"
        #define macro GET(aaa) = takes(0) returns(1) {
            <aaa>
        }

        #define macro ADD(bbb, ccc) = takes(2) returns(1) {
            <bbb> <ccc> add
        }

        #define macro RUN(ddd, eee) = takes(0) returns(1) {
            ADD(GET(<ddd>), <eee>)
        }

        #define macro MAIN() = takes(0) returns(1) {
            RUN(0x01, 0x02)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x01, PUSH1 0x02, ADD
    assert_eq!(main_bytecode.to_lowercase(), "6001600201");
}

#[test]
fn test_deeply_nested_argcalls() {
    // Complex case: Multiple levels of nesting with ArgCalls at different levels
    // Pattern: GrandParent(Parent(Child(<arg1>), <arg2>), <arg3>)
    let source = r#"
        #define macro GET(aaa) = takes(0) returns(1) {
            <aaa>
        }

        #define macro ADD(bbb, ccc) = takes(2) returns(1) {
            <bbb> <ccc> add
        }

        #define macro STORE(operation, offset) = takes(0) returns(0) {
            <operation> <offset> mstore
        }

        #define macro RUN(ddd, eee, fff) = takes(0) returns(0) {
            STORE(ADD(GET(<ddd>), <eee>), <fff>)
        }

        #define macro MAIN() = takes(0) returns(0) {
            RUN(0x01, 0x02, 0x00)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x01, PUSH1 0x02, ADD, PUSH1 0x00, MSTORE
    assert_eq!(main_bytecode.to_lowercase(), "60016002015f52");
}

#[test]
fn test_argcall_scope_chain_traversal() {
    // Tests that ArgCalls properly traverse scope chain to find definitions
    // When <arg> is not found in immediate macro, it should bubble up to parent scopes
    let source = r#"
        #define macro INNER(param) = takes(0) returns(1) {
            <param>
        }

        #define macro MIDDLE() = takes(0) returns(1) {
            INNER(<outer_param>)  // outer_param not defined in MIDDLE, should bubble to OUTER
        }

        #define macro OUTER(outer_param) = takes(0) returns(1) {
            MIDDLE()
        }

        #define macro MAIN() = takes(0) returns(1) {
            OUTER(0x42)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x42
    assert_eq!(main_bytecode.to_lowercase(), "6042");
}

#[test]
fn test_undefined_argcall_in_macrocall_errors() {
    // Ensures that truly undefined ArgCalls within MacroCalls still error appropriately
    let source = r#"
        #define macro GET(aaa) = takes(0) returns(1) {
            <undefined_arg>  // This arg is not defined anywhere
        }

        #define macro RUN(ddd) = takes(0) returns(1) {
            GET(<ddd>)
        }

        #define macro MAIN() = takes(0) returns(1) {
            RUN(0x01)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false);

    assert!(result.is_err());
    assert_eq!(result.unwrap_err().kind, CodegenErrorKind::MissingArgumentDefinition(String::from("undefined_arg")));
}

#[test]
fn test_nested_macro_call_with_argcall() {
    // Pattern: RUN(arg1, arg2) -> MUL(ADD_TWO(<arg1>), <arg2>)
    let source = r#"
        #define macro ADD_TWO(val) = takes(0) returns(1) {
            <val> 0x02 add
        }

        #define macro MUL(a, b) = takes(2) returns(1) {
            <a> <b> mul
        }

        #define macro RUN(arg1, arg2) = takes(0) returns(1) {
            MUL(ADD_TWO(<arg1>), <arg2>)
        }

        #define macro MAIN() = takes(0) returns(1) {
            RUN(0x05, 0x03)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: PUSH1 0x05, PUSH1 0x02, ADD, PUSH1 0x03, MUL
    assert_eq!(main_bytecode.to_lowercase(), "6005600201600302");
}

#[test]
fn test_nested_macro_with_label_argument() {
    // Nested macro invocation with label argument
    // Pattern: WRAPPER(JUMPER(label)) where label is defined in parent
    let source = r#"
        #define macro JUMP_TO(lbl) = takes(0) returns(1) {
            <lbl> jump
        }

        #define macro MACRO(inner) = takes(0) returns(0) {
            <inner>
        }

        #define macro MAIN() = takes(0) returns(0) {
            jmp:
            MACRO(JUMP_TO(jmp))
            0x00 0x00 revert
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: JUMPDEST, PUSH2 0x0000, JUMP, PUSH0, PUSH0, REVERT
    assert_eq!(main_bytecode.to_lowercase(), "5b610000565f5ffd");
}

#[test]
fn test_label_before_nested_macro_invocation() {
    // Label defined before nested macro invocation
    // Pattern: label defined, then WRAPPER(JUMPER(label))
    let source = r#"
        #define macro JUMP_TO(lbl) = takes(0) returns(0) {
            <lbl> jump
        }

        #define macro WRAPPER(inner) = takes(0) returns(0) {
            <inner>
        }

        #define macro MAIN() = takes(0) returns(0) {
            target:
            0x01
            WRAPPER(JUMP_TO(target))
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: JUMPDEST, PUSH1 0x01, PUSH2 0x0000, JUMP
    assert_eq!(main_bytecode.to_lowercase(), "5b600161000056");
}

#[test]
fn test_deeply_nested_macro_with_label_argument() {
    // Deeply nested macro invocations with labels
    // Pattern: LEVEL2(LEVEL1(JUMPER(label)))
    let source = r#"
        #define macro JUMP(lbl) = takes(0) returns(0) {
            <lbl> jump
        }

        #define macro LEVEL1(arg1) = takes(0) returns(0) {
            <arg1>
        }

        #define macro LEVEL2(arg2) = takes(0) returns(0) {
            <arg2>
        }

        #define macro MAIN() = takes(0) returns(0) {
            dest:
            LEVEL2(LEVEL1(JUMP(dest)))
            0x00 0x00 revert
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let main_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Expected: JUMPDEST, PUSH2 0x0000, JUMP, PUSH0, PUSH0, REVERT
    assert_eq!(main_bytecode.to_lowercase(), "5b610000565f5ffd");
}
