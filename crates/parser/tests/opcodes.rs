use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn not_mistaken_as_opcode() {
    for opcode in OPCODES {
        let label = format!("{}_label", (*opcode).to_owned());
        let source = &format!(
            r#"
                #define macro IS_AUTHORIZED(some_arg) = takes(0) returns(0) {{}}
                #define macro MAIN() = takes(0) returns(0) {{
                    IS_AUTHORIZED({label})
                    {label}:
                        return
                }}
            "#
        );
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);

        let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

        let actual_label_arg = tokens[tokens.len() - 7].kind.clone();
        let actual_label = tokens[tokens.len() - 5].kind.clone();
        let mut parser = Parser::new(tokens, None);
        // parsing to ensure tokens syntax is valid
        let _contract = parser.parse().unwrap();
        assert_eq!(actual_label_arg, TokenKind::Ident(label.clone()));
        assert_eq!(actual_label, TokenKind::Label(label));
    }
}

#[test]
#[should_panic]
fn test_invalid_push_non_literal() {
    let source: &str = r#"
        // Here we have a macro invocation directly in the parameter list - this should fail
        #define macro MAIN() = takes (0) returns (0) {
            push1 0x10
            push32 0x108
            push1 push1
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Should fail here
    parser.parse().unwrap();
}

#[test]
fn test_push_literals() {
    let source: &str = r#"
        #define macro MAIN() = {
            push1 0x10
            push32 0x108
            push1 0x10 0x10
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();

    let expected_tokens = vec![
        Token { kind: TokenKind::Whitespace, span: Span { start: 0, end: 9, file: None } },
        Token { kind: TokenKind::Define, span: Span { start: 9, end: 16, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 16, end: 17, file: None } },
        Token { kind: TokenKind::Macro, span: Span { start: 17, end: 22, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 22, end: 23, file: None } },
        Token { kind: TokenKind::Ident("MAIN".to_string()), span: Span { start: 23, end: 27, file: None } },
        Token { kind: TokenKind::OpenParen, span: Span { start: 27, end: 28, file: None } },
        Token { kind: TokenKind::CloseParen, span: Span { start: 28, end: 29, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 29, end: 30, file: None } },
        Token { kind: TokenKind::Assign, span: Span { start: 30, end: 31, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 31, end: 32, file: None } },
        Token { kind: TokenKind::OpenBrace, span: Span { start: 32, end: 33, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 33, end: 46, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push1), span: Span { start: 46, end: 51, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 51, end: 52, file: None } },
        Token { kind: TokenKind::HexLiteral("10".to_string()), span: Span { start: 52, end: 56, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 56, end: 69, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push32), span: Span { start: 69, end: 75, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 75, end: 76, file: None } },
        Token { kind: TokenKind::HexLiteral("108".to_string()), span: Span { start: 76, end: 81, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 81, end: 94, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push1), span: Span { start: 94, end: 99, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 99, end: 100, file: None } },
        Token { kind: TokenKind::HexLiteral("10".to_string()), span: Span { start: 100, end: 104, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 104, end: 105, file: None } },
        Token { kind: TokenKind::HexLiteral("10".to_string()), span: Span { start: 105, end: 109, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 109, end: 118, file: None } },
        Token { kind: TokenKind::CloseBrace, span: Span { start: 118, end: 119, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 119, end: 124, file: None } },
        Token { kind: TokenKind::Eof, span: Span { start: 124, end: 124, file: None } },
    ];
    assert_eq!(expected_tokens, tokens);

    // This should parse correctly
    let mut parser = Parser::new(tokens, None);
    parser.parse().unwrap();
}

#[test]
fn test_push0() {
    let source: &str = r#"
        #define macro MAIN() = {
            push1 0x10
            push32 0x108
            push1 0x10 0x10
            push0
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();

    let expected_tokens = vec![
        Token { kind: TokenKind::Whitespace, span: Span { start: 0, end: 9, file: None } },
        Token { kind: TokenKind::Define, span: Span { start: 9, end: 16, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 16, end: 17, file: None } },
        Token { kind: TokenKind::Macro, span: Span { start: 17, end: 22, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 22, end: 23, file: None } },
        Token { kind: TokenKind::Ident("MAIN".to_string()), span: Span { start: 23, end: 27, file: None } },
        Token { kind: TokenKind::OpenParen, span: Span { start: 27, end: 28, file: None } },
        Token { kind: TokenKind::CloseParen, span: Span { start: 28, end: 29, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 29, end: 30, file: None } },
        Token { kind: TokenKind::Assign, span: Span { start: 30, end: 31, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 31, end: 32, file: None } },
        Token { kind: TokenKind::OpenBrace, span: Span { start: 32, end: 33, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 33, end: 46, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push1), span: Span { start: 46, end: 51, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 51, end: 52, file: None } },
        Token { kind: TokenKind::HexLiteral("10".to_string()), span: Span { start: 52, end: 56, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 56, end: 69, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push32), span: Span { start: 69, end: 75, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 75, end: 76, file: None } },
        Token { kind: TokenKind::HexLiteral("108".to_string()), span: Span { start: 76, end: 81, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 81, end: 94, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push1), span: Span { start: 94, end: 99, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 99, end: 100, file: None } },
        Token { kind: TokenKind::HexLiteral("10".to_string()), span: Span { start: 100, end: 104, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 104, end: 105, file: None } },
        Token { kind: TokenKind::HexLiteral("10".to_string()), span: Span { start: 105, end: 109, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 109, end: 122, file: None } },
        Token { kind: TokenKind::Opcode(Opcode::Push0), span: Span { start: 122, end: 127, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 127, end: 136, file: None } },
        Token { kind: TokenKind::CloseBrace, span: Span { start: 136, end: 137, file: None } },
        Token { kind: TokenKind::Whitespace, span: Span { start: 137, end: 142, file: None } },
        Token { kind: TokenKind::Eof, span: Span { start: 142, end: 142, file: None } },
    ];
    assert_eq!(expected_tokens, tokens);

    // This should parse correctly
    let mut parser = Parser::new(tokens, None);
    parser.parse().unwrap();
}

#[test]
fn test_keccak256_and_sha3_opcodes() {
    // Test that both keccak256 and sha3 are recognized as valid opcodes
    let source: &str = r#"
        #define macro MAIN() = {
            0x00 0x00 keccak256
            0x00 0x00 sha3
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();

    // Both should be recognized as the Keccak256 opcode
    let keccak256_token = tokens
        .iter()
        .find(|t| matches!(t.kind, TokenKind::Opcode(Opcode::Keccak256)))
        .expect("keccak256 should be recognized as Keccak256 opcode");
    assert!(matches!(keccak256_token.kind, TokenKind::Opcode(Opcode::Keccak256)));

    // This should parse correctly
    let mut parser = Parser::new(tokens, None);
    parser.parse().unwrap();
}
