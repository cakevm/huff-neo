use huff_neo_lexer::Lexer;
use huff_neo_parser::*;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_parses_decorator_argument_kinds() {
    // Test that every kind of decorator argument value is parsed, and that hex literals keep their width
    let source = r#"#[flag(0x0001, 42, "0x1234", true, false, [SLOT], this, at = 0x1000, __ERROR(Unauthorized))] #define test T() = {}"#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();

    let decorator = contract.macros.get("T").unwrap().decorator.clone().unwrap();
    assert_eq!(
        decorator.flags,
        vec![DecoratorFlag {
            name: String::from("flag"),
            args: vec![
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Hex(String::from("0001")),
                    span: AstSpan(vec![Span { start: 7, end: 13, file: None }]),
                },
                DecoratorArg { key: None, value: DecoratorValue::Int(42), span: AstSpan(vec![Span { start: 15, end: 17, file: None }]) },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Str(String::from("0x1234")),
                    span: AstSpan(vec![Span { start: 19, end: 27, file: None }]),
                },
                DecoratorArg { key: None, value: DecoratorValue::Bool(true), span: AstSpan(vec![Span { start: 29, end: 33, file: None }]) },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Bool(false),
                    span: AstSpan(vec![Span { start: 35, end: 40, file: None }])
                },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Constant(String::from("SLOT")),
                    span: AstSpan(vec![Span { start: 42, end: 48, file: None }]),
                },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Ident(String::from("this")),
                    span: AstSpan(vec![Span { start: 50, end: 54, file: None }]),
                },
                DecoratorArg {
                    key: Some(String::from("at")),
                    value: DecoratorValue::Hex(String::from("1000")),
                    span: AstSpan(vec![Span { start: 56, end: 67, file: None }]),
                },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Builtin(BuiltinFunctionCall {
                        kind: BuiltinFunctionKind::Error,
                        args: vec![BuiltinFunctionArg::Identifier(
                            String::from("Unauthorized"),
                            AstSpan(vec![Span { start: 77, end: 89, file: None }]),
                        )],
                        span: AstSpan(vec![Span { start: 69, end: 76, file: None }]),
                    }),
                    span: AstSpan(vec![Span { start: 69, end: 90, file: None }]),
                },
            ],
            span: AstSpan(vec![Span { start: 2, end: 6, file: None }]),
        }]
    );
}

#[test]
fn test_parses_stacked_decorator_blocks() {
    // Test that stacked decorator blocks are merged in source order, the same as a single block
    let stacked = "#[caller(0xbeef)] #[expect_revert,] #define test T() = {}";
    let flattened_source = FullFileSource { source: stacked, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let stacked_flags = parser.parse().unwrap().macros.get("T").unwrap().decorator.clone().unwrap().flags;

    assert_eq!(
        stacked_flags,
        vec![
            DecoratorFlag {
                name: String::from("caller"),
                args: vec![DecoratorArg {
                    key: None,
                    value: DecoratorValue::Hex(String::from("beef")),
                    span: AstSpan(vec![Span { start: 9, end: 15, file: None }]),
                }],
                span: AstSpan(vec![Span { start: 2, end: 8, file: None }]),
            },
            DecoratorFlag {
                name: String::from("expect_revert"),
                args: vec![],
                span: AstSpan(vec![Span { start: 20, end: 33, file: None }])
            },
        ]
    );

    let single = "#[caller(0xbeef), expect_revert,] #define test T() = {}";
    let flattened_source = FullFileSource { source: single, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let single_flags = parser.parse().unwrap().macros.get("T").unwrap().decorator.clone().unwrap().flags;

    // Spans differ between the two sources, so only names and values are compared
    let names_and_values = |flags: &[DecoratorFlag]| -> Vec<(String, Vec<DecoratorValue>)> {
        flags.iter().map(|f| (f.name.clone(), f.args.iter().map(|a| a.value.clone()).collect())).collect()
    };
    assert_eq!(names_and_values(&stacked_flags), names_and_values(&single_flags));
}

#[test]
fn test_parses_decorator_flags_without_commas() {
    // Test that flags without a comma between them still parse, for compatibility with older sources
    let source = "#[caller(0x01) expect_revert] #define test T() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();

    let decorator = contract.macros.get("T").unwrap().decorator.clone().unwrap();
    assert_eq!(
        decorator.flags,
        vec![
            DecoratorFlag {
                name: String::from("caller"),
                args: vec![DecoratorArg {
                    key: None,
                    value: DecoratorValue::Hex(String::from("01")),
                    span: AstSpan(vec![Span { start: 9, end: 13, file: None }]),
                }],
                span: AstSpan(vec![Span { start: 2, end: 8, file: None }]),
            },
            DecoratorFlag {
                name: String::from("expect_revert"),
                args: vec![],
                span: AstSpan(vec![Span { start: 15, end: 28, file: None }])
            },
        ]
    );
}

#[test]
fn test_parses_keywords_as_decorator_identifiers() {
    // Test that keywords such as `returns`, `indexed` and `macro` are plain identifiers inside a decorator
    let source = "#[returns(indexed, macro)] #define test T() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();

    let decorator = contract.macros.get("T").unwrap().decorator.clone().unwrap();
    assert_eq!(
        decorator.flags,
        vec![DecoratorFlag {
            name: String::from("returns"),
            args: vec![
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Ident(String::from("indexed")),
                    span: AstSpan(vec![Span { start: 10, end: 17, file: None }]),
                },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Ident(String::from("macro")),
                    span: AstSpan(vec![Span { start: 19, end: 24, file: None }]),
                },
            ],
            span: AstSpan(vec![Span { start: 2, end: 9, file: None }]),
        }]
    );
}

#[test]
fn test_parses_decorator_after_abi_and_constant_definitions() {
    // Test that a decorator ends a function or constant definition, which have no terminal token
    let source = "#define function transfer(address,uint256) nonpayable returns ()
#[calldata(__FUNC_SIG(transfer))]
#define test A() = {}
#define constant K = 0x01
#[store([K], 0x02)]
#define test B() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();

    let decorator_a = contract.macros.get("A").unwrap().decorator.clone().unwrap();
    assert_eq!(
        decorator_a.flags,
        vec![DecoratorFlag {
            name: String::from("calldata"),
            args: vec![DecoratorArg {
                key: None,
                value: DecoratorValue::Builtin(BuiltinFunctionCall {
                    kind: BuiltinFunctionKind::FunctionSignature,
                    args: vec![BuiltinFunctionArg::Identifier(
                        String::from("transfer"),
                        AstSpan(vec![Span { start: 87, end: 95, file: None }])
                    )],
                    span: AstSpan(vec![Span { start: 76, end: 86, file: None }]),
                }),
                span: AstSpan(vec![Span { start: 76, end: 96, file: None }]),
            }],
            span: AstSpan(vec![Span { start: 67, end: 75, file: None }]),
        }]
    );

    let decorator_b = contract.macros.get("B").unwrap().decorator.clone().unwrap();
    assert_eq!(
        decorator_b.flags,
        vec![DecoratorFlag {
            name: String::from("store"),
            args: vec![
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Constant(String::from("K")),
                    span: AstSpan(vec![Span { start: 155, end: 158, file: None }]),
                },
                DecoratorArg {
                    key: None,
                    value: DecoratorValue::Hex(String::from("02")),
                    span: AstSpan(vec![Span { start: 160, end: 164, file: None }]),
                },
            ],
            span: AstSpan(vec![Span { start: 149, end: 154, file: None }]),
        }]
    );
}

#[test]
fn test_rejects_decorator_on_non_test_macro() {
    // Test that a decorator above a macro or fn is an error pointing at the whole decorator
    for kind in ["macro", "fn"] {
        let source = &format!("#[value(0x01)]\n#define {kind} M() = {{}}");
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);
        let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
        let mut parser = Parser::new(tokens, None);
        let result = parser.parse();

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind, ParserErrorKind::DecoratorOnNonTest(String::from("M")));
        assert_eq!(err.spans, AstSpan(vec![Span { start: 0, end: 14, file: None }]));
        assert!(err.hint.unwrap().contains("#define test"));
    }
}

#[test]
fn test_rejects_non_identifier_decorator_flag() {
    // Test that a decorator flag must start with a name, also between flags without commas
    for source in ["#[0x01] #define test T() = {}", "#[caller(0x01) 0x02] #define test T() = {}"] {
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);
        let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
        let mut parser = Parser::new(tokens, None);
        let result = parser.parse();

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlag(_)));
        assert!(err.hint.unwrap().contains("Expected a decorator flag name"));
    }
}

#[test]
fn test_rejects_missing_comma_between_decorator_args() {
    // Test that arguments of a decorator flag must be comma separated
    let source = "#[store(0x01 0x02)] #define test T() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlag(_)));
    assert!(err.hint.unwrap().contains("Expected `,` or `)`"));
}

#[test]
fn test_rejects_invalid_decorator_argument() {
    // Test that a token that cannot be a decorator argument value is rejected
    let source = "#[caller(:)] #define test T() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlagArg(TokenKind::Colon)));
    assert!(err.hint.unwrap().contains("caller"));
}
