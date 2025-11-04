use huff_neo_lexer::*;
use huff_neo_utils::prelude::*;

#[test]
fn parses_if_basic() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(condition) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    let _ = lexer.next(); // #define
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // macro
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // TEST
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
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace

    // Lex 'if' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let if_span = Span::new(45..47, None);
    assert_eq!(unwrapped, Token::new(TokenKind::If, if_span));

    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // condition (identifier)
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn parses_if_else() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(x) { } else { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    let _ = lexer.next(); // #define
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // macro
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // TEST
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
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // if
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // x (identifier)
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace

    // Lex 'else' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let else_span = Span::new(55..59, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Else, else_span));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn parses_comparison_equal_equal() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(a == b) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Skip to the comparison operator (26 tokens before ==)
    for _ in 0..26 {
        let _ = lexer.next();
    }

    // Lex '==' operator
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let eq_span = Span::new(50..52, None);
    assert_eq!(unwrapped, Token::new(TokenKind::EqualEqual, eq_span));

    // Continue lexing to ensure rest is valid
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_comparison_not_equal() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(a != b) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Skip to the comparison operator (26 tokens before !=)
    for _ in 0..26 {
        let _ = lexer.next();
    }

    // Lex '!=' operator
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let ne_span = Span::new(50..52, None);
    assert_eq!(unwrapped, Token::new(TokenKind::NotEqual, ne_span));

    // Continue lexing to ensure rest is valid
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_comparison_less_equal() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(a <= b) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Skip to the comparison operator (26 tokens before <=)
    for _ in 0..26 {
        let _ = lexer.next();
    }

    // Lex '<=' operator
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let le_span = Span::new(50..52, None);
    assert_eq!(unwrapped, Token::new(TokenKind::LessEqual, le_span));

    // Continue lexing to ensure rest is valid
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_comparison_greater_equal() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(a >= b) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Skip to the comparison operator (26 tokens before >=)
    for _ in 0..26 {
        let _ = lexer.next();
    }

    // Lex '>=' operator
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let ge_span = Span::new(50..52, None);
    assert_eq!(unwrapped, Token::new(TokenKind::GreaterEqual, ge_span));

    // Continue lexing to ensure rest is valid
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_logical_not() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(!flag) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Skip to the logical NOT operator (24 tokens before !)
    for _ in 0..24 {
        let _ = lexer.next();
    }

    // Lex '!' operator
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let not_span = Span::new(48..49, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Not, not_span));

    // Continue lexing to ensure rest is valid
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_if_nested() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(x) { if(y) { } } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_if_with_newlines() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
        if(condition) {
        }
    }"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_if_tight_spacing() {
    let source = "#define macro TEST() = takes(0) returns(0) {if(x){}}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}
