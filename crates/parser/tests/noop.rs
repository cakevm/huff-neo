use huff_neo_lexer::*;
use huff_neo_parser::*;
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
    assert_eq!(noop_constant.name, "MY_NOOP");
    assert_eq!(noop_constant.value, ConstVal::Noop);
    assert_eq!(noop_constant.span.0.len(), 5); // Has 5 spans
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
