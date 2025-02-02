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
fn constant_uneven_hex_literal_length() {
    let source = "#define constant TEST = 0x1";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens[0].kind, TokenKind::Define);
    assert_eq!(tokens[1].kind, TokenKind::Constant);
    assert_eq!(tokens[2].kind, TokenKind::Ident("TEST".to_string()));
    assert_eq!(tokens[3].kind, TokenKind::Assign);
    assert_eq!(tokens[4].kind, TokenKind::Bytes("01".to_string()));
}

#[test]
fn constant_leading_zeros_hex_literal() {
    let source = "#define constant TEST = 0x000000000000000000000000000000000000000000000000000000010203";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens[0].kind, TokenKind::Define);
    assert_eq!(tokens[1].kind, TokenKind::Constant);
    assert_eq!(tokens[2].kind, TokenKind::Ident("TEST".to_string()));
    assert_eq!(tokens[3].kind, TokenKind::Assign);
    assert_eq!(tokens[4].kind, TokenKind::Bytes("000000000000000000000000000000000000000000000000000000010203".to_string()));
}

#[test]
fn constant_builtin() {
    let source = "#define constant TEST = __FUNC_SIG('hello()')";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens[0].kind, TokenKind::Define);
    assert_eq!(tokens[1].kind, TokenKind::Constant);
    assert_eq!(tokens[2].kind, TokenKind::Ident("TEST".to_string()));
    assert_eq!(tokens[3].kind, TokenKind::Assign);
    assert_eq!(tokens[4].kind, TokenKind::BuiltinFunction("__FUNC_SIG".to_string()));
    assert_eq!(tokens[5].kind, TokenKind::OpenParen);
    assert_eq!(tokens[6].kind, TokenKind::Str("hello()".to_string()));
    assert_eq!(tokens[7].kind, TokenKind::CloseParen);
}
