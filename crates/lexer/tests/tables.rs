use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn parses_jump_table() {
    let source = "#define jumptable JUMP_TABLE()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens.first().unwrap().kind, TokenKind::Define);
    assert_eq!(tokens.get(1).unwrap().kind, TokenKind::JumpTable);
    assert_eq!(tokens.get(2).unwrap().kind, TokenKind::Ident(String::from("JUMP_TABLE")));
    assert_eq!(tokens.get(3).unwrap().kind, TokenKind::OpenParen);
    assert_eq!(tokens.get(4).unwrap().kind, TokenKind::CloseParen);
}

#[test]
fn parses_packed_jump_table() {
    let source = "#define jumptable__packed JUMP_TABLE_PACKED()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens.first().unwrap().kind, TokenKind::Define);
    assert_eq!(tokens.get(1).unwrap().kind, TokenKind::JumpTablePacked);
    assert_eq!(tokens.get(2).unwrap().kind, TokenKind::Ident(String::from("JUMP_TABLE_PACKED")));
    assert_eq!(tokens.get(3).unwrap().kind, TokenKind::OpenParen);
    assert_eq!(tokens.get(4).unwrap().kind, TokenKind::CloseParen);
}

#[test]
fn parses_code_table() {
    let source = "#define table CODE_TABLE()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens.first().unwrap().kind, TokenKind::Define);
    assert_eq!(tokens.get(1).unwrap().kind, TokenKind::CodeTable);
    assert_eq!(tokens.get(2).unwrap().kind, TokenKind::Ident(String::from("CODE_TABLE")));
    assert_eq!(tokens.get(3).unwrap().kind, TokenKind::OpenParen);
    assert_eq!(tokens.get(4).unwrap().kind, TokenKind::CloseParen);
}

#[test]
fn parse_code_table_with_func_sign() {
    let source = r"#define table CODE_TABLE() {
        __FUNC_SIG('hello()')
        0x123
    }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

    assert_eq!(tokens.first().unwrap().kind, TokenKind::Define);
    assert_eq!(tokens.get(1).unwrap().kind, TokenKind::CodeTable);
    assert_eq!(tokens.get(2).unwrap().kind, TokenKind::Ident(String::from("CODE_TABLE")));
    assert_eq!(tokens.get(3).unwrap().kind, TokenKind::OpenParen);
    assert_eq!(tokens.get(4).unwrap().kind, TokenKind::CloseParen);
    assert_eq!(tokens.get(5).unwrap().kind, TokenKind::OpenBrace);
    assert_eq!(tokens.get(6).unwrap().kind, TokenKind::BuiltinFunction("__FUNC_SIG".to_string()));
    assert_eq!(tokens.get(7).unwrap().kind, TokenKind::OpenParen);
    assert_eq!(tokens.get(8).unwrap().kind, TokenKind::Str(String::from("hello()")));
    assert_eq!(tokens.get(9).unwrap().kind, TokenKind::CloseParen);
    assert_eq!(tokens.get(10).unwrap().kind, TokenKind::Ident("123".to_string()));
    assert_eq!(tokens.get(11).unwrap().kind, TokenKind::CloseBrace);
    assert_eq!(tokens.get(12).unwrap().kind, TokenKind::Eof);
}
