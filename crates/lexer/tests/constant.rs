use huff_neo_lexer::Lexer;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::LexicalErrorKind;

#[test]
fn constant_hex_literal_too_long() {
    let source = "#define constant TEST = 0x000000000000000000000000000000000000000000000000000000000000000000";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    for tok in lexer {
        if tok.is_err() {
            assert_eq!(
                tok.unwrap_err().kind,
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
