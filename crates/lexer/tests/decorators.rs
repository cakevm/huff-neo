use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::{Span, Token, TokenKind};

#[test]
fn parses_decorator() {
    let key_words = ["macro", "fn", "test"];

    for s in key_words {
        let source = &format!(
            r#"
            #[calldata(0x01)]
            #define {} NUMS() = takes(0) returns(1) {}
                0x00 dup1 mstore
            {}
            "#,
            s, "{", "}",
        );

        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let mut lexer = Lexer::new(flattened_source);

        let _ = lexer.next(); // whitespace

        // #
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(13..14, None);
        assert_eq!(unwrapped, Token::new(TokenKind::Pound, returns_span.clone()));

        // [
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(14..15, None);
        assert_eq!(unwrapped, Token::new(TokenKind::OpenBracket, returns_span.clone()));

        // calldata
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(15..23, None);
        assert_eq!(unwrapped, Token::new(TokenKind::Ident(String::from("calldata")), returns_span.clone()));

        // (
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(23..24, None);
        assert_eq!(unwrapped, Token::new(TokenKind::OpenParen, returns_span.clone()));

        // 0x01 hex literal
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(24..28, None);
        assert_eq!(unwrapped, Token::new(TokenKind::HexLiteral("01".to_string()), returns_span.clone()));

        // )
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(28..29, None);
        assert_eq!(unwrapped, Token::new(TokenKind::CloseParen, returns_span.clone()));

        // ]
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let returns_span = Span::new(29..30, None);
        assert_eq!(unwrapped, Token::new(TokenKind::CloseBracket, returns_span.clone()));

        let _ = lexer.next(); // whitespace'
        let _ = lexer.next(); // define
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // body type
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // NUMS
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // =
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // takes
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // 0
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // returns
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // 1
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // {
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // 0x00
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // dup1
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // mstore
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // }
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // eof

        // We covered the whole source
        assert!(lexer.eof);
    }
}

#[test]
fn parses_decorator_after_definition_without_terminal_token() {
    // Constant and ABI definitions have no closing token, so the decorator must end them
    let definitions = [
        "#define constant NUM = 0x01",
        "#define function foo(uint256) view returns (uint256)",
        "#define event Foo(uint256)",
        "#define error Bar(uint256)",
    ];

    for d in definitions {
        let source = &format!(
            r#"
            {d}
            #[calldata(0x01)]
            #define test NUMS() = takes(0) returns(1) {{
                0x00 dup1 mstore
            }}
            "#,
        );

        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);
        let tokens = lexer.into_iter().collect::<Result<Vec<Token>, _>>().unwrap();
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Pound), "no decorator after `{d}`");
    }
}

#[test]
#[should_panic]
fn fails_to_parse_decorator_in_body() {
    let key_words = ["macro", "fn", "test"];

    for s in key_words {
        let source = &format!(
            r#"
            #define {} NUMS() = takes(0) returns(1) {}
                #[calldata(0x01)]
                0x00 dup1 mstore
            {}
            "#,
            s, "{", "}",
        );

        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let mut lexer = Lexer::new(flattened_source);

        for token in lexer.by_ref() {
            if let Err(e) = token {
                panic!("{e:?}");
            }
        }
    }
}

#[test]
fn parses_builtin_functions_in_decorator() {
    // Builtin calls are decorator arguments, so nested builtins must lex as builtin functions
    let source = "#[calldata(__RIGHTPAD(__FUNC_SIG(transfer)))]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens =
        lexer.into_iter().map(|x| x.unwrap()).filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Eof)).collect::<Vec<Token>>();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Pound, Span::new(0..1, None)),
            Token::new(TokenKind::OpenBracket, Span::new(1..2, None)),
            Token::new(TokenKind::Ident(String::from("calldata")), Span::new(2..10, None)),
            Token::new(TokenKind::OpenParen, Span::new(10..11, None)),
            Token::new(TokenKind::BuiltinFunction(String::from("__RIGHTPAD")), Span::new(11..21, None)),
            Token::new(TokenKind::OpenParen, Span::new(21..22, None)),
            Token::new(TokenKind::BuiltinFunction(String::from("__FUNC_SIG")), Span::new(22..32, None)),
            Token::new(TokenKind::OpenParen, Span::new(32..33, None)),
            Token::new(TokenKind::Ident(String::from("transfer")), Span::new(33..41, None)),
            Token::new(TokenKind::CloseParen, Span::new(41..42, None)),
            Token::new(TokenKind::CloseParen, Span::new(42..43, None)),
            Token::new(TokenKind::CloseParen, Span::new(43..44, None)),
            Token::new(TokenKind::CloseBracket, Span::new(44..45, None)),
        ]
    );
}

#[test]
fn parses_keywords_as_identifiers_in_decorator() {
    // Decorator flags and arguments may use keyword names such as `returns` or `indexed`
    let source = "#[returns(indexed, macro)]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens =
        lexer.into_iter().map(|x| x.unwrap()).filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Eof)).collect::<Vec<Token>>();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Pound, Span::new(0..1, None)),
            Token::new(TokenKind::OpenBracket, Span::new(1..2, None)),
            Token::new(TokenKind::Ident(String::from("returns")), Span::new(2..9, None)),
            Token::new(TokenKind::OpenParen, Span::new(9..10, None)),
            Token::new(TokenKind::Ident(String::from("indexed")), Span::new(10..17, None)),
            Token::new(TokenKind::Comma, Span::new(17..18, None)),
            Token::new(TokenKind::Ident(String::from("macro")), Span::new(19..24, None)),
            Token::new(TokenKind::CloseParen, Span::new(24..25, None)),
            Token::new(TokenKind::CloseBracket, Span::new(25..26, None)),
        ]
    );
}

#[test]
fn parses_definition_after_decorator_with_constant() {
    // The closing bracket of a `[CONST]` argument must not end the decorator, only its final bracket
    let source = "#[store([K], 0x01)] #define test T() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens =
        lexer.into_iter().map(|x| x.unwrap()).filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Eof)).collect::<Vec<Token>>();

    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Pound, Span::new(0..1, None)),
            Token::new(TokenKind::OpenBracket, Span::new(1..2, None)),
            Token::new(TokenKind::Ident(String::from("store")), Span::new(2..7, None)),
            Token::new(TokenKind::OpenParen, Span::new(7..8, None)),
            Token::new(TokenKind::OpenBracket, Span::new(8..9, None)),
            Token::new(TokenKind::Ident(String::from("K")), Span::new(9..10, None)),
            Token::new(TokenKind::CloseBracket, Span::new(10..11, None)),
            Token::new(TokenKind::Comma, Span::new(11..12, None)),
            Token::new(TokenKind::HexLiteral(String::from("01")), Span::new(13..17, None)),
            Token::new(TokenKind::CloseParen, Span::new(17..18, None)),
            Token::new(TokenKind::CloseBracket, Span::new(18..19, None)),
            Token::new(TokenKind::Define, Span::new(20..27, None)),
            Token::new(TokenKind::Test, Span::new(28..32, None)),
            Token::new(TokenKind::Ident(String::from("T")), Span::new(33..34, None)),
            Token::new(TokenKind::OpenParen, Span::new(34..35, None)),
            Token::new(TokenKind::CloseParen, Span::new(35..36, None)),
            Token::new(TokenKind::Assign, Span::new(37..38, None)),
            Token::new(TokenKind::OpenBrace, Span::new(39..40, None)),
            Token::new(TokenKind::CloseBrace, Span::new(40..41, None)),
        ]
    );
}
