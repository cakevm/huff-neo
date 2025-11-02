use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn noop_in_macro_body() {
    let source = r#"#define macro TEST() = takes(0) returns(0) {
            __NOOP
        }"#;
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

    // The __NOOP builtin should be parsed as TokenKind::Noop here
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let noop_span = Span::new(57..63, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Noop, noop_span));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn noop_with_other_opcodes() {
    let source = "#define macro TEST() = takes(0) returns(0) { 0x01 __NOOP dup1 __NOOP }";
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
    let _ = lexer.next(); // 0x01
    let _ = lexer.next(); // whitespace

    // First __NOOP
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let noop_span = Span::new(50..56, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Noop, noop_span));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // dup1
    let _ = lexer.next(); // whitespace

    // Second __NOOP
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let noop_span2 = Span::new(62..68, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Noop, noop_span2));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn noop_in_for_loop() {
    let source = r#"#define macro TEST() = takes(0) returns(0) { for(i in 0..5) {
                __NOOP
            } }"#;
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
    let _ = lexer.next(); // 5
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace

    // The __NOOP builtin should work in ForLoopBody context
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let noop_span = Span::new(78..84, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Noop, noop_span));

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn noop_in_constant_context() {
    let source = "#define constant MY_NOOP = __NOOP";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    let _ = lexer.next(); // #define
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // constant
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // MY_NOOP
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // =
    let _ = lexer.next(); // whitespace

    // The __NOOP builtin should be parsed as TokenKind::Noop in constant context
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let noop_span = Span::new(27..33, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Noop, noop_span));

    let _ = lexer.next(); // eof

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
#[should_panic]
fn fails_to_parse_noop_outside_valid_context() {
    // __NOOP should only be lexed in MacroBody, ForLoopBody, or Constant contexts
    let source = "__NOOP";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    // This should NOT be TokenKind::Noop in top-level context
    assert_eq!(unwrapped, Token::new(TokenKind::Noop, Span::new(0..6, None)));

    let _ = lexer.next(); // eof
    assert!(lexer.eof);
}
