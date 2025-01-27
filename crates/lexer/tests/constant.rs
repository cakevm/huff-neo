use huff_neo_lexer::Lexer;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::{LexicalErrorKind, Token, TokenKind};

#[test]
fn constant_hex_literal_too_long() {
    let source = "#define constant TEST = 0x000000000000000000000000000000000000000000000000000000000000000000";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    for token_result in lexer {
        if token_result.is_err() {
            assert_eq!(
                token_result.unwrap_err().kind,
                LexicalErrorKind::HexLiteralTooLong("0x000000000000000000000000000000000000000000000000000000000000000000".to_string())
            );
            return;
        }
    }
    panic!("Error did not occurred")
}

#[test]
fn constant_invalid_hex_literal() {
    let source = "#define constant TEST = 0x0x";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    for tok in lexer {
        if tok.is_err() {
            assert_eq!(tok.unwrap_err().kind, LexicalErrorKind::InvalidHexLiteral("0x0x".to_string()));
            return;
        }
    }
    panic!("Error did not occurred")
}

#[test]
fn constant_leading_zeros_hex_literal() {
    let source = "#define constant TEST = 0x0000000000000000000000000000000000000000000000000000000010203";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| matches!(x.kind, TokenKind::Literal { .. })).collect::<Vec<Token>>();

    let TokenKind::Literal(value) = tokens.first().unwrap().kind else { panic!("Expected Literal token") };
    assert_eq!(value, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3]);
}
