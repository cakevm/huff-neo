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

#[test]
fn parses_bool() {
    let source = r#"
    #define constant CONSTANT_FALSE = false
    #define constant CONSTANT_TRUE = true

    #define macro CHECK_BOOL() {
        false
        true
    }
    "#;

    let flattened_source = FullFileSource { source: source.trim(), file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);

    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    // Constant false
    assert_eq!(tokens[0].kind, TokenKind::Define);
    assert_eq!(tokens[1].kind, TokenKind::Constant);
    assert_eq!(tokens[2].kind, TokenKind::Ident("CONSTANT_FALSE".to_string()));
    assert_eq!(tokens[3].kind, TokenKind::Assign);
    assert_eq!(tokens[4].kind, TokenKind::Literal(str_to_bytes32("0")));

    // Constant true
    assert_eq!(tokens[5].kind, TokenKind::Define);
    assert_eq!(tokens[6].kind, TokenKind::Constant);
    assert_eq!(tokens[7].kind, TokenKind::Ident("CONSTANT_TRUE".to_string()));
    assert_eq!(tokens[8].kind, TokenKind::Assign);
    assert_eq!(tokens[9].kind, TokenKind::Literal(str_to_bytes32("1")));

    // Macro
    assert_eq!(tokens[10].kind, TokenKind::Define);
    assert_eq!(tokens[11].kind, TokenKind::Macro);
    assert_eq!(tokens[12].kind, TokenKind::Ident("CHECK_BOOL".to_string()));
    assert_eq!(tokens[13].kind, TokenKind::OpenParen);
    assert_eq!(tokens[14].kind, TokenKind::CloseParen);
    assert_eq!(tokens[15].kind, TokenKind::OpenBrace);
    assert_eq!(tokens[16].kind, TokenKind::Literal(str_to_bytes32("0")));
    assert_eq!(tokens[17].kind, TokenKind::Literal(str_to_bytes32("1")));
    assert_eq!(tokens[18].kind, TokenKind::CloseBrace);
    assert_eq!(tokens[19].kind, TokenKind::Eof);
}

#[test]
fn parse_true_false_name() {
    let source = r#"
    #define constant true = true
    #define constant false = false

    #define macro true() {
        true
    }
    
    #define macro false() {
        false
    }
    "#;

    let flattened_source = FullFileSource { source: source.trim(), file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);

    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    // Constant false
    assert_eq!(tokens[0].kind, TokenKind::Define);
    assert_eq!(tokens[1].kind, TokenKind::Constant);
    assert_eq!(tokens[2].kind, TokenKind::Ident("true".to_string()));
    assert_eq!(tokens[3].kind, TokenKind::Assign);
    assert_eq!(tokens[4].kind, TokenKind::Literal(str_to_bytes32("1")));

    // Constant true
    assert_eq!(tokens[5].kind, TokenKind::Define);
    assert_eq!(tokens[6].kind, TokenKind::Constant);
    assert_eq!(tokens[7].kind, TokenKind::Ident("false".to_string()));
    assert_eq!(tokens[8].kind, TokenKind::Assign);
    assert_eq!(tokens[9].kind, TokenKind::Literal(str_to_bytes32("0")));

    // Macro true
    assert_eq!(tokens[10].kind, TokenKind::Define);
    assert_eq!(tokens[11].kind, TokenKind::Macro);
    assert_eq!(tokens[12].kind, TokenKind::Ident("true".to_string()));
    assert_eq!(tokens[13].kind, TokenKind::OpenParen);
    assert_eq!(tokens[14].kind, TokenKind::CloseParen);
    assert_eq!(tokens[15].kind, TokenKind::OpenBrace);
    assert_eq!(tokens[16].kind, TokenKind::Literal(str_to_bytes32("1")));
    assert_eq!(tokens[17].kind, TokenKind::CloseBrace);

    // Macro false
    assert_eq!(tokens[18].kind, TokenKind::Define);
    assert_eq!(tokens[19].kind, TokenKind::Macro);
    assert_eq!(tokens[20].kind, TokenKind::Ident("false".to_string()));
    assert_eq!(tokens[21].kind, TokenKind::OpenParen);
    assert_eq!(tokens[22].kind, TokenKind::CloseParen);
    assert_eq!(tokens[23].kind, TokenKind::OpenBrace);
    assert_eq!(tokens[24].kind, TokenKind::Literal(str_to_bytes32("0")));
    assert_eq!(tokens[25].kind, TokenKind::CloseBrace);

    assert_eq!(tokens[26].kind, TokenKind::Eof);
}
