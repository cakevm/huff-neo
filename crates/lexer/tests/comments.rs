use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn instantiates() {
    let source = "#define macro HELLO_WORLD()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    assert!(!lexer.eof);
}

#[test]
fn single_line_comments() {
    let source = "// comment contents \n#define macro HELLO_WORLD()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first token should be a single line comment
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped, Token::new(TokenKind::Comment("// comment contents ".to_string()), Span::new(0..20, None)));

    // The second token should be the newline character parsed as a whitespace
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(20..21, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Whitespace, define_span));

    // This token should be a Define identifier
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(21..28, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Define, define_span));

    // The next token should be the whitespace
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(28..29, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Whitespace, define_span));

    // Then we should parse the macro keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let macro_span = Span::new(29..34, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Macro, macro_span));

    // The next token should be another whitespace
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let ws_span = Span::new(34..35, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Whitespace, ws_span));

    // Then we should get the function name
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let function_span = Span::new(35..46, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Ident("HELLO_WORLD".to_string()), function_span));

    // Then we should have an open paren
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let open_paren_span = Span::new(46..47, None);
    assert_eq!(unwrapped, Token::new(TokenKind::OpenParen, open_paren_span));

    // Lastly, we should have a closing parenthesis
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let close_paren_span = Span::new(47..48, None);
    assert_eq!(unwrapped, Token::new(TokenKind::CloseParen, close_paren_span));

    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let eof_span = Span::new(48..48, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Eof, eof_span));

    // We covered the whole source
    assert!(lexer.eof);
    assert_eq!(source.len() - 1, 47);
}

#[test]
fn multi_line_comments() {
    let source = "/* comment contents*/#define macro HELLO_WORLD()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first token should be a single line comment
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped, Token::new(TokenKind::Comment("/* comment contents*/".to_string()), Span::new(0..21, None)));

    // This token should be a Define identifier
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(21..28, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Define, define_span));

    // The next token should be the whitespace
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(28..29, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Whitespace, define_span));

    // Then we should parse the macro keyword
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let macro_span = Span::new(29..34, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Macro, macro_span));

    // The next token should be another whitespace
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let ws_span = Span::new(34..35, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Whitespace, ws_span));

    // Then we should get the function name
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let function_span = Span::new(35..46, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Ident("HELLO_WORLD".to_string()), function_span));

    // Then we should have an open paren
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let open_paren_span = Span::new(46..47, None);
    assert_eq!(unwrapped, Token::new(TokenKind::OpenParen, open_paren_span));

    // Lastly, we should have a closing parenthesis
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let close_paren_span = Span::new(47..48, None);
    assert_eq!(unwrapped, Token::new(TokenKind::CloseParen, close_paren_span));

    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let eof_span = Span::new(48..48, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Eof, eof_span));

    // We covered the whole source
    assert!(lexer.eof);
    assert_eq!(source.len() - 1, 47);
}

#[test]
fn single_line_comment_with_utf8_arrow() {
    // Test that UTF-8 arrow character in comments doesn't break subsequent token spans
    // Arrow (→) is 3 bytes (UTF-8: e2 86 92)
    let source = "// a → b\n#define macro X() = takes(0) returns(0) {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Comment: "// a → b" = 2 + 1 + 1 + 1 + 3 + 1 + 1 = 10 bytes (positions 0-10)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let comment_span = Span::new(0..10, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Comment("// a → b".to_string()), comment_span));

    // The next token should be the newline character parsed as whitespace
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let ws_span = Span::new(10..11, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Whitespace, ws_span));

    // #define should start at byte 11, not byte 9 (verifies UTF-8 byte positions are correct)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(11..18, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Define, define_span));
}

#[test]
fn multi_line_comment_with_utf8_arrow() {
    // Test that UTF-8 arrow character in block comments doesn't break subsequent token spans
    // Arrow (→) is 3 bytes (UTF-8: e2 86 92)
    let source = "/* a → b */#define macro X() = takes(0) returns(0) {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Comment: "/* a → b */" = 2 + 1 + 1 + 1 + 3 + 1 + 1 + 1 + 2 = 13 bytes (positions 0-13)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let comment_span = Span::new(0..13, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Comment("/* a → b */".to_string()), comment_span));

    // #define should start at byte 13, not byte 11 (verifies UTF-8 byte positions are correct)
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    let define_span = Span::new(13..20, None);
    assert_eq!(unwrapped, Token::new(TokenKind::Define, define_span));
}
