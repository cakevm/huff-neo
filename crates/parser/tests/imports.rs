use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn parses_import() {
    let source = " /* .,*./. */  #include \"../../resources/erc20/ERC20.huff\"";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let import_path = contract.imports[0].clone();
    assert_eq!(import_path.to_str().unwrap(), "../../resources/erc20/ERC20.huff");
}

#[test]
fn parses_deep_imports() {
    let source = r#"
        /* .,*./. */
        #include "../../resources/erc20/ERC20.huff"
        #define macro MAIN() = takes (0) returns (0) { /* Mock Empty Main Macro */ }
        #include "../../resources/erc20/utils/Ownable.huff"
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let import_path = contract.imports[0].clone();
    assert_eq!(import_path.to_str().unwrap(), "../../resources/erc20/ERC20.huff");
}
