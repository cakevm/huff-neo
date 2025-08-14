use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn lexes_zero_prefixed_numbers() {
    let source = "00";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first and only token should be lexed as 0
    let tok = lexer.next().unwrap().unwrap();
    assert_eq!(tok, Token::new(TokenKind::Num(0), Span::new(0..2, None)));

    lexer.next();

    // We covered the whole source
    assert!(lexer.eof);
}

#[test]
fn lexes_large_numbers() {
    let source = &format!("{}", usize::MAX);
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // The first and only token should be lexed
    let tok = lexer.next().unwrap().unwrap();
    assert_eq!(tok, Token::new(TokenKind::Num(usize::MAX), Span::new(0..source.len(), None)));

    lexer.next();

    // We covered the whole source
    assert!(lexer.eof);
}
