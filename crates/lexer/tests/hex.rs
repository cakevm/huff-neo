use huff_neo_lexer::Lexer;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn parses_single_hex() {
    let source = "0xa57B";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first and only token should be lexed as Literal(0xa57B)
    let tok = lexer.next().unwrap().unwrap();
    assert_eq!(tok, Token::new(TokenKind::Literal(str_to_bytes32("a57B")), Span::new(2..5, None)));

    // We covered the whole source
    lexer.next();
    assert!(lexer.eof);
}

#[test]
fn parses_bool() {
    let source = "false true";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first token should be lexed as a Literal representing 0x00
    let tok = lexer.next().unwrap().unwrap();
    assert_eq!(tok, Token::new(TokenKind::Literal(str_to_bytes32("0")), Span::new(0..4, None)));

    let _ = lexer.next(); // Whitespace

    // The second token should be lexed as a Literal representing 0x01
    let tok = lexer.next().unwrap().unwrap();
    assert_eq!(tok, Token::new(TokenKind::Literal(str_to_bytes32("1")), Span::new(6..9, None)));

    // We covered the whole source
    lexer.next();
    assert!(lexer.eof);
}

#[test]
fn parses_odd_len_hex() {
    let source = "0x1";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first and only token should be lexed as Literal(0x1)
    let tok = lexer.next().unwrap().unwrap();
    assert_eq!(tok, Token::new(TokenKind::Literal(str_to_bytes32("1")), Span::new(2..2, None)));

    // We covered the whole source
    lexer.next();
    assert!(lexer.eof);
}
