use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::{opcodes::Opcode, prelude::*};

#[test]
fn test_parses_noop_constant() {
    let source = "#define constant MY_NOOP = __NOOP";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let noop_constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(
        noop_constant,
        ConstantDefinition {
            name: "MY_NOOP".to_string(),
            value: ConstVal::Noop,
            span: AstSpan(vec![
                Span { start: 0, end: 7, file: None },   // #define
                Span { start: 8, end: 16, file: None },  // constant
                Span { start: 17, end: 24, file: None }, // MY_NOOP
                Span { start: 25, end: 26, file: None }, // =
                Span { start: 27, end: 33, file: None }  // __NOOP
            ])
        }
    );
}

#[test]
fn test_rejects_noop_as_constant_name() {
    let source = "#define constant __NOOP = 0x00";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    // Should get a helpful error that __NOOP is reserved
    assert!(matches!(err.kind, ParserErrorKind::InvalidConstantName));
    assert!(err.hint.unwrap().contains("reserved"));
}

#[test]
fn test_parses_noop_in_macro_body() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
            __NOOP
            0x01
            __NOOP
        }"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let test_macro = contract.macros.iter().find(|m| m.1.name == "TEST").unwrap().1;

    // __NOOP should not create any statements, so we should only have the 0x01 literal
    assert_eq!(test_macro.statements.len(), 1);
    assert!(matches!(test_macro.statements[0].ty, StatementType::Literal(_)));
    assert_eq!(test_macro.statements[0].ty, StatementType::Literal(str_to_bytes32("01")));
}

#[test]
fn test_parses_noop_in_for_loop() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
            for(i in 0..3) {
                __NOOP
                dup1
            }
        }"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let test_macro = contract.macros.iter().find(|m| m.1.name == "TEST").unwrap().1;

    // Should have one for loop statement
    assert_eq!(test_macro.statements.len(), 1);

    if let StatementType::ForLoop { body, variable, start, end, step } = &test_macro.statements[0].ty {
        // Inside the for loop, __NOOP should be skipped, only dup1 remains
        assert_eq!(body.len(), 1);
        assert_eq!(variable, "i");
        assert!(step.is_none());

        // Check the dup1 opcode
        assert_eq!(body[0].ty, StatementType::Opcode(Opcode::Dup1));

        // Check start and end are literals
        assert!(matches!(start, Expression::Literal { .. }));
        assert!(matches!(end, Expression::Literal { .. }));
    } else {
        panic!("Expected ForLoop statement");
    }
}

#[test]
fn test_parses_noop_in_label() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
            my_label:
                __NOOP
                0x01
                __NOOP
        }"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let test_macro = contract.macros.iter().find(|m| m.1.name == "TEST").unwrap().1;

    // Should have one label statement
    assert_eq!(test_macro.statements.len(), 1);

    if let StatementType::Label(label) = &test_macro.statements[0].ty {
        // Inside the label, __NOOP should be skipped, only the literal remains
        assert_eq!(label.inner.len(), 1);
        assert_eq!(label.name, "my_label");
        assert_eq!(label.inner[0].ty, StatementType::Literal(str_to_bytes32("01")));
    } else {
        panic!("Expected Label statement");
    }
}

#[test]
fn test_parses_multiple_noops_in_sequence() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
            __NOOP
            __NOOP
            0x42
            __NOOP
        }"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let test_macro = contract.macros.iter().find(|m| m.1.name == "TEST").unwrap().1;

    // All __NOOP instances should be skipped, only 0x42 should remain
    assert_eq!(test_macro.statements.len(), 1);
    assert_eq!(test_macro.statements[0].ty, StatementType::Literal(str_to_bytes32("42")));
}

#[test]
fn test_parses_noop_as_macro_argument() {
    // Test that __NOOP works as a macro argument
    let source = r#"
        #define macro MACRO(arg) = takes(0) returns(0) {
            <arg>
            0x42
        }

        #define macro MAIN() = takes(0) returns(0) {
            MACRO(__NOOP)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    // Get the MAIN macro
    let main_macro = contract.macros.iter().find(|m| m.1.name == "MAIN").unwrap().1;

    // Should have one MacroInvocation statement
    assert_eq!(main_macro.statements.len(), 1);

    // Check that MACRO(__NOOP) is parsed correctly
    if let StatementType::MacroInvocation(mi) = &main_macro.statements[0].ty {
        assert_eq!(mi.macro_name, "MACRO");
        assert_eq!(mi.args.len(), 1);

        // The critical assertion: __NOOP should be parsed as MacroArg::Noop, not MacroArg::Ident
        assert!(matches!(mi.args[0], MacroArg::Noop));

        // Check the span of the MacroInvocation includes all tokens
        assert_eq!(
            mi.span,
            AstSpan(vec![
                Span { start: 169, end: 174, file: None }, // MACRO
                Span { start: 174, end: 175, file: None }, // (
                Span { start: 175, end: 181, file: None }, // __NOOP
                Span { start: 181, end: 182, file: None }, // )
            ])
        );
    } else {
        panic!("Expected MacroInvocation for MACRO");
    }
}

#[test]
fn test_parses_noop_with_other_macro_args() {
    // Test __NOOP mixed with other types of arguments
    let source = r#"
        #define macro MACRO(arg1, arg2, arg3) = takes(0) returns(0) {
            <arg1>
            <arg2>
            <arg3>
        }

        #define macro MAIN() = takes(0) returns(0) {
            MACRO(0x01, __NOOP, 0x02)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    // Get the MAIN macro
    let main_macro = contract.macros.iter().find(|m| m.1.name == "MAIN").unwrap().1;

    // Should have one MacroInvocation statement
    assert_eq!(main_macro.statements.len(), 1);

    // Check that MACRO(0x01, __NOOP, 0x02) is parsed correctly
    if let StatementType::MacroInvocation(mi) = &main_macro.statements[0].ty {
        assert_eq!(mi.macro_name, "MACRO");
        assert_eq!(mi.args.len(), 3);

        // First argument should be a literal
        if let MacroArg::HexLiteral(bytes) = &mi.args[0] {
            assert_eq!(bytes[31], 0x01);
        } else {
            panic!("Expected literal 0x01");
        }

        // Second argument should be Noop
        assert!(matches!(mi.args[1], MacroArg::Noop));

        // Third argument should be a literal
        if let MacroArg::HexLiteral(bytes) = &mi.args[2] {
            assert_eq!(bytes[31], 0x02);
        } else {
            panic!("Expected literal 0x02");
        }

        // Check the span includes all tokens
        assert_eq!(
            mi.span,
            AstSpan(vec![
                Span { start: 204, end: 209, file: None }, // MACRO
                Span { start: 209, end: 210, file: None }, // (
                Span { start: 210, end: 214, file: None }, // 0x01
                Span { start: 214, end: 215, file: None }, // ,
                Span { start: 216, end: 222, file: None }, // __NOOP
                Span { start: 222, end: 223, file: None }, // ,
                Span { start: 224, end: 228, file: None }, // 0x02
                Span { start: 228, end: 229, file: None }, // )
            ])
        );
    } else {
        panic!("Expected MacroInvocation for MACRO");
    }
}

#[test]
fn test_parses_noop_in_arg_macro_invocation() {
    // Test __NOOP as an argument to an arg macro invocation <m>(__NOOP)
    let source = r#"
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>(__NOOP, 0x01)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let wrapper_macro = contract.macros.iter().find(|m| m.1.name == "WRAPPER").unwrap().1;

    // Should have one ArgMacroInvocation statement
    assert_eq!(wrapper_macro.statements.len(), 1);

    // Check that <m>(__NOOP, 0x01) is parsed correctly
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &wrapper_macro.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 2);

        // First argument should be Noop
        assert!(matches!(args[0], MacroArg::Noop));

        // Second argument should be a literal
        if let MacroArg::HexLiteral(bytes) = &args[1] {
            assert_eq!(bytes[31], 0x01);
        } else {
            panic!("Expected literal 0x01");
        }

        // Check span of the statement
        assert_eq!(
            wrapper_macro.statements[0].span,
            AstSpan(vec![
                Span { start: 70, end: 73, file: None }, // <m>
            ])
        );
    } else {
        panic!("Expected ArgMacroInvocation");
    }
}
