use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::ast::abi::Argument;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_parses_free_storage_pointer_constant() {
    let source = "#define constant FSP_LOCATION = FREE_STORAGE_POINTER()";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let fsp_constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(
        fsp_constant,
        ConstantDefinition {
            name: "FSP_LOCATION".to_string(),
            value: ConstVal::FreeStoragePointer(FreeStoragePointer {}),
            span: AstSpan(vec![
                Span { start: 0, end: 6, file: None },
                Span { start: 8, end: 15, file: None },
                Span { start: 17, end: 28, file: None },
                Span { start: 30, end: 30, file: None },
                Span { start: 32, end: 53, file: None }
            ])
        }
    );
}

#[test]
fn test_parses_literal_constant() {
    let source = "#define constant LITERAL = 0x0000E1E5EBEC7D5BD14F71427D1E84F3DD0314C0F7B2291E5B200AC8C7C3B925";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    // Check Literal
    let fsp_constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(
        fsp_constant,
        ConstantDefinition {
            name: "LITERAL".to_string(),
            value: ConstVal::Bytes(Bytes("0000e1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925".to_string())),
            span: AstSpan(vec![
                Span { start: 0, end: 6, file: None },
                Span { start: 8, end: 15, file: None },
                Span { start: 17, end: 23, file: None },
                Span { start: 25, end: 25, file: None },
                Span { start: 29, end: 92, file: None }
            ])
        }
    );
}

#[test]
fn test_parses_func_sign_constant() {
    let source = "#define constant FUNC = __FUNC_SIG('hello()')";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    // Check Literal
    let fsp_constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(
        fsp_constant,
        ConstantDefinition {
            name: "FUNC".to_string(),
            value: ConstVal::BuiltinFunctionCall(BuiltinFunctionCall {
                kind: BuiltinFunctionKind::FunctionSignature,
                args: vec![BuiltinFunctionArg::Argument(Argument {
                    arg_type: None,
                    arg_location: None,
                    name: Some("hello()".to_string()),
                    indexed: false,
                    span: AstSpan(vec![Span { start: 35, end: 43, file: None }])
                })],
                span: AstSpan(vec![Span { start: 24, end: 33, file: None }]),
            }),
            span: AstSpan(vec![
                Span { start: 0, end: 6, file: None },
                Span { start: 8, end: 15, file: None },
                Span { start: 17, end: 20, file: None },
                Span { start: 22, end: 22, file: None },
                Span { start: 24, end: 33, file: None },
                Span { start: 34, end: 34, file: None },
                Span { start: 35, end: 43, file: None },
                Span { start: 44, end: 44, file: None }
            ])
        }
    );
}
