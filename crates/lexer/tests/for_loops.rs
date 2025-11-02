use huff_neo_lexer::*;
use huff_neo_utils::prelude::*;

#[test]
fn parses_for_loop_basic() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..5) { } }";
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

    // Lex 'for' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let for_span = Span::new(45..48, None);
    assert_eq!(unwrapped, Token::new(TokenKind::For, for_span));

    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // i (identifier)
    let _ = lexer.next(); // whitespace

    // Lex 'in' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let in_span = Span::new(51..53, None);
    assert_eq!(unwrapped, Token::new(TokenKind::In, in_span));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 0

    // Lex '..' (DoubleDot)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let doubledot_span = Span::new(55..57, None);
    assert_eq!(unwrapped, Token::new(TokenKind::DoubleDot, doubledot_span));

    let _ = lexer.next(); // 5
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
fn parses_for_loop_with_step() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..10 step 2) { } }";
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
    let _ = lexer.next(); // for
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // i
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // in
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // ..
    let _ = lexer.next(); // 10
    let _ = lexer.next(); // whitespace

    // Lex 'step' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let step_span = Span::new(60..64, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Step, step_span));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 2
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
fn parses_for_loop_nested() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..2) { for(j in 0..2) { } } }";
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

    // First 'for' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::For);

    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // i
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // in
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // ..
    let _ = lexer.next(); // 2
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace

    // Second 'for' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::For);

    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // j
    let _ = lexer.next(); // whitespace

    // Second 'in' keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::In);

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // ..
    let _ = lexer.next(); // 2
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_with_variable_interpolation() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..3) { <i> } }";
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
    let _ = lexer.next(); // for
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // i
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // in
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // ..
    let _ = lexer.next(); // 3
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace

    // Lex '<' (LeftAngle)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::LeftAngle);

    // Lex 'i' (identifier)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::Ident("i".to_string()));

    // Lex '>' (RightAngle)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::RightAngle);

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_with_hex_bounds() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0x00..0xFF) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_with_constants() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in START..END) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_tight_spacing() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..10) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_loose_spacing() {
    let source = "#define macro TEST() = takes(0) returns(0) { for( i in 0 .. 10 ) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_empty_body() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..5) {} }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}

#[test]
fn parses_for_loop_with_newlines() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
    for(i in 0..3) {
        <i>
    }
}"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Just verify it lexes without error
    while let Some(Ok(_)) = lexer.next() {}
    assert!(lexer.eof);
}
