use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::ast::abi::Argument;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::{error::ParserErrorKind, opcodes::Opcode, prelude::*};

#[test]
fn multiline_labels() {
    let source = r#"
    #define macro HELLO_WORLD() = takes(3) returns(0) {
      0x00 mstore
      0x01 0x02 add
      cool_label:
        HELLO()
        0x00 0x00 revert
    }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let macro_definition = parser.parse().unwrap().macros.get("HELLO_WORLD").cloned().unwrap();
    let md_expected = MacroDefinition {
        name: "HELLO_WORLD".to_string(),
        decorator: None,
        parameters: vec![],
        statements: vec![
            Statement { ty: StatementType::Literal(str_to_bytes32("00")), span: AstSpan(vec![Span { start: 63, end: 67, file: None }]) },
            Statement { ty: StatementType::Opcode(Opcode::Mstore), span: AstSpan(vec![Span { start: 68, end: 74, file: None }]) },
            Statement { ty: StatementType::Literal(str_to_bytes32("01")), span: AstSpan(vec![Span { start: 81, end: 85, file: None }]) },
            Statement { ty: StatementType::Literal(str_to_bytes32("02")), span: AstSpan(vec![Span { start: 86, end: 90, file: None }]) },
            Statement { ty: StatementType::Opcode(Opcode::Add), span: AstSpan(vec![Span { start: 91, end: 94, file: None }]) },
            Statement {
                ty: StatementType::Label(Label {
                    name: "cool_label".to_string(),
                    inner: vec![
                        Statement {
                            ty: StatementType::MacroInvocation(MacroInvocation {
                                macro_name: "HELLO".to_string(),
                                args: vec![],
                                span: AstSpan(vec![
                                    Span { start: 121, end: 126, file: None },
                                    Span { start: 126, end: 127, file: None },
                                    Span { start: 127, end: 128, file: None },
                                ]),
                            }),
                            span: AstSpan(vec![
                                Span { start: 121, end: 126, file: None },
                                Span { start: 126, end: 127, file: None },
                                Span { start: 127, end: 128, file: None },
                            ]),
                        },
                        Statement {
                            ty: StatementType::Literal(str_to_bytes32("00")),
                            span: AstSpan(vec![Span { start: 137, end: 141, file: None }]),
                        },
                        Statement {
                            ty: StatementType::Literal(str_to_bytes32("00")),
                            span: AstSpan(vec![Span { start: 142, end: 146, file: None }]),
                        },
                        Statement {
                            ty: StatementType::Opcode(Opcode::Revert),
                            span: AstSpan(vec![Span { start: 147, end: 153, file: None }]),
                        },
                    ],
                    span: AstSpan(vec![
                        Span { start: 101, end: 111, file: None },
                        Span { start: 121, end: 126, file: None },
                        Span { start: 126, end: 127, file: None },
                        Span { start: 127, end: 128, file: None },
                        Span { start: 137, end: 141, file: None },
                        Span { start: 142, end: 146, file: None },
                        Span { start: 147, end: 153, file: None },
                    ]),
                }),
                span: AstSpan(vec![
                    Span { start: 101, end: 111, file: None },
                    Span { start: 121, end: 126, file: None },
                    Span { start: 126, end: 127, file: None },
                    Span { start: 127, end: 128, file: None },
                    Span { start: 137, end: 141, file: None },
                    Span { start: 142, end: 146, file: None },
                    Span { start: 147, end: 153, file: None },
                ]),
            },
        ],
        takes: 3,
        returns: 0,
        span: AstSpan(vec![
            // "#define"
            Span { start: 5, end: 12, file: None },
            // "macro"
            Span { start: 13, end: 18, file: None },
            // "HELLO_WORLD"
            Span { start: 19, end: 30, file: None },
            // "("
            Span { start: 30, end: 31, file: None },
            // ")"
            Span { start: 31, end: 32, file: None },
            // "="
            Span { start: 33, end: 34, file: None },
            // "takes"
            Span { start: 35, end: 40, file: None },
            // "("
            Span { start: 40, end: 41, file: None },
            // "0"
            Span { start: 41, end: 42, file: None },
            // ")"
            Span { start: 42, end: 43, file: None },
            // "returns"
            Span { start: 44, end: 51, file: None },
            // "("
            Span { start: 51, end: 52, file: None },
            // "0"
            Span { start: 52, end: 53, file: None },
            // ")"
            Span { start: 53, end: 54, file: None },
            // "{"
            Span { start: 55, end: 56, file: None },
            // "0x00"
            Span { start: 63, end: 67, file: None },
            // "mstore"
            Span { start: 68, end: 74, file: None },
            // "0x01"
            Span { start: 81, end: 85, file: None },
            // "0x02"
            Span { start: 86, end: 90, file: None },
            // "add"
            Span { start: 91, end: 94, file: None },
            // "cool_label"
            Span { start: 101, end: 111, file: None },
            // ":"
            Span { start: 111, end: 112, file: None },
            // "HELLO"
            Span { start: 121, end: 126, file: None },
            // "("
            Span { start: 126, end: 127, file: None },
            // ")"
            Span { start: 127, end: 128, file: None },
            // "0x00"
            Span { start: 137, end: 141, file: None },
            // "0x00"
            Span { start: 142, end: 146, file: None },
            // "revert"
            Span { start: 147, end: 153, file: None },
            // "}"
            Span { start: 158, end: 159, file: None },
        ]),
        outlined: false,
        test: false,
    };
    assert_eq!(macro_definition.name, md_expected.name);
    assert_eq!(macro_definition.parameters, md_expected.parameters);
    assert_eq!(macro_definition.takes, md_expected.takes);
    assert_eq!(macro_definition.returns, md_expected.returns);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
    assert_eq!(macro_definition.span, md_expected.span);

    // Test that each statement is the correct type
    for (i, s) in macro_definition.statements.iter().enumerate() {
        assert_eq!(s.ty, md_expected.statements[i].ty);
        assert_eq!(s.span, md_expected.statements[i].span);
    }
}

#[test]
pub fn builtins_under_labels() {
    let source = r#"
    #define function myFunc() pure returns (uint256)

    #define error TestError()

    #define jumptable__packed TEST_TABLE {
        my_label
    }

    #define macro SMALL_MACRO() = takes (3) returns (0) {
        0x20 0x00 mstore
    }

    #define macro HELLO_WORLD() = takes(3) returns(0) {
        my_label:
            __tablestart(TEST_TABLE)
            __tablesize(TEST_TABLE)
            __codesize(SMALL_MACRO)
            __FUNC_SIG(myFunc)
            __ERROR(TestError)
            __RIGHTPAD(0xBB)
    }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let macro_definition = parser.parse().unwrap().macros.get("HELLO_WORLD").cloned().unwrap();
    let md_expected = MacroDefinition {
        name: "HELLO_WORLD".to_string(),
        parameters: vec![],
        decorator: None,
        statements: vec![Statement {
            ty: StatementType::Label(Label {
                name: String::from("my_label"),
                inner: vec![
                    Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::Tablestart,
                            args: vec![BuiltinFunctionArg::Identifier(
                                String::from("TEST_TABLE"),
                                AstSpan(vec![Span { start: 342, end: 352, file: None }]),
                            )],
                            span: AstSpan(vec![Span { start: 329, end: 341, file: None }, Span { start: 342, end: 352, file: None }]),
                        }),
                        span: AstSpan(vec![Span { start: 329, end: 341, file: None }, Span { start: 342, end: 352, file: None }]),
                    },
                    Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::Tablesize,
                            args: vec![BuiltinFunctionArg::Identifier(
                                String::from("TEST_TABLE"),
                                AstSpan(vec![Span { start: 378, end: 388, file: None }]),
                            )],
                            span: AstSpan(vec![Span { start: 366, end: 377, file: None }, Span { start: 378, end: 388, file: None }]),
                        }),
                        span: AstSpan(vec![Span { start: 366, end: 377, file: None }, Span { start: 378, end: 388, file: None }]),
                    },
                    Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::Codesize,
                            args: vec![BuiltinFunctionArg::Identifier(
                                String::from("SMALL_MACRO"),
                                AstSpan(vec![Span { start: 413, end: 424, file: None }]),
                            )],
                            span: AstSpan(vec![Span { start: 402, end: 412, file: None }, Span { start: 413, end: 424, file: None }]),
                        }),
                        span: AstSpan(vec![Span { start: 402, end: 412, file: None }, Span { start: 413, end: 424, file: None }]),
                    },
                    Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::FunctionSignature,
                            args: vec![BuiltinFunctionArg::Identifier(
                                String::from("myFunc"),
                                AstSpan(vec![Span { start: 449, end: 455, file: None }]),
                            )],
                            span: AstSpan(vec![Span { start: 438, end: 448, file: None }, Span { start: 449, end: 455, file: None }]),
                        }),
                        span: AstSpan(vec![Span { start: 438, end: 448, file: None }, Span { start: 449, end: 455, file: None }]),
                    },
                    Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::Error,
                            args: vec![BuiltinFunctionArg::Identifier(
                                String::from("TestError"),
                                AstSpan(vec![Span { start: 477, end: 486, file: None }]),
                            )],
                            span: AstSpan(vec![Span { start: 469, end: 476, file: None }, Span { start: 477, end: 486, file: None }]),
                        }),
                        span: AstSpan(vec![Span { start: 469, end: 476, file: None }, Span { start: 477, end: 486, file: None }]),
                    },
                    Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::RightPad,
                            args: vec![BuiltinFunctionArg::HexLiteral(
                                "bb".to_string(),
                                AstSpan(vec![Span { start: 511, end: 515, file: None }]),
                            )],
                            span: AstSpan(vec![Span { start: 500, end: 510, file: None }, Span { start: 511, end: 515, file: None }]),
                        }),
                        span: AstSpan(vec![Span { start: 500, end: 510, file: None }, Span { start: 511, end: 515, file: None }]),
                    },
                ],
                span: AstSpan(vec![
                    Span { start: 307, end: 315, file: None },
                    Span { start: 329, end: 341, file: None },
                    Span { start: 342, end: 352, file: None },
                    Span { start: 366, end: 377, file: None },
                    Span { start: 378, end: 388, file: None },
                    Span { start: 402, end: 412, file: None },
                    Span { start: 413, end: 424, file: None },
                    Span { start: 438, end: 448, file: None },
                    Span { start: 449, end: 455, file: None },
                    Span { start: 469, end: 476, file: None },
                    Span { start: 477, end: 486, file: None },
                    Span { start: 500, end: 510, file: None },
                    Span { start: 511, end: 515, file: None },
                ]),
            }),
            span: AstSpan(vec![
                Span { start: 307, end: 315, file: None },
                Span { start: 329, end: 341, file: None },
                Span { start: 342, end: 352, file: None },
                Span { start: 366, end: 377, file: None },
                Span { start: 378, end: 388, file: None },
                Span { start: 402, end: 412, file: None },
                Span { start: 413, end: 424, file: None },
                Span { start: 438, end: 448, file: None },
                Span { start: 449, end: 455, file: None },
                Span { start: 469, end: 476, file: None },
                Span { start: 477, end: 486, file: None },
                Span { start: 500, end: 510, file: None },
                Span { start: 511, end: 515, file: None },
            ]),
        }],
        takes: 3,
        returns: 0,
        span: AstSpan(vec![
            Span { start: 247, end: 254, file: None },
            Span { start: 255, end: 260, file: None },
            Span { start: 261, end: 272, file: None },
            Span { start: 272, end: 273, file: None },
            Span { start: 273, end: 274, file: None },
            Span { start: 275, end: 276, file: None },
            Span { start: 277, end: 282, file: None },
            Span { start: 282, end: 283, file: None },
            Span { start: 283, end: 284, file: None },
            Span { start: 284, end: 285, file: None },
            Span { start: 286, end: 293, file: None },
            Span { start: 293, end: 294, file: None },
            Span { start: 294, end: 295, file: None },
            Span { start: 295, end: 296, file: None },
            Span { start: 297, end: 298, file: None },
            Span { start: 307, end: 315, file: None },
            Span { start: 315, end: 316, file: None },
            Span { start: 329, end: 341, file: None },
            Span { start: 341, end: 342, file: None },
            Span { start: 342, end: 352, file: None },
            Span { start: 352, end: 353, file: None },
            Span { start: 366, end: 377, file: None },
            Span { start: 377, end: 378, file: None },
            Span { start: 378, end: 388, file: None },
            Span { start: 388, end: 389, file: None },
            Span { start: 402, end: 412, file: None },
            Span { start: 412, end: 413, file: None },
            Span { start: 413, end: 424, file: None },
            Span { start: 424, end: 425, file: None },
            Span { start: 438, end: 448, file: None },
            Span { start: 448, end: 449, file: None },
            Span { start: 449, end: 455, file: None },
            Span { start: 455, end: 456, file: None },
            Span { start: 469, end: 476, file: None },
            Span { start: 476, end: 477, file: None },
            Span { start: 477, end: 486, file: None },
            Span { start: 486, end: 487, file: None },
            Span { start: 500, end: 510, file: None },
            Span { start: 510, end: 511, file: None },
            Span { start: 511, end: 515, file: None },
            Span { start: 515, end: 516, file: None },
            Span { start: 521, end: 522, file: None },
        ]),
        outlined: false,
        test: false,
    };
    assert_eq!(macro_definition.name, md_expected.name);
    assert_eq!(macro_definition.parameters, md_expected.parameters);
    assert_eq!(macro_definition.takes, md_expected.takes);
    assert_eq!(macro_definition.returns, md_expected.returns);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
    assert_eq!(macro_definition.span, md_expected.span);

    // Test that each statement is the correct type
    for (i, s) in macro_definition.statements.iter().enumerate() {
        assert_eq!(s.ty, md_expected.statements[i].ty);
        assert_eq!(s.span, md_expected.statements[i].span);
    }
}

#[test]
fn duplicated_labels() {
    let source = r#"
    #define macro MAIN() = takes(0) returns(0) {
        cool_label jump
        cool_label jump
        cool_label: 0x00
        dup_label: 0x00
        dup_label: 0x00
    }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Grab the first macro
    let parse_result = parser.parse();
    assert!(parse_result.is_err());
    assert_eq!(parse_result.unwrap_err().kind, ParserErrorKind::DuplicateLabel("dup_label".to_string()));
}

#[test]
fn label_with_first_class_macro_invocation() {
    let source = r#"
    #define macro WRAPPER(m) = takes(0) returns(0) {
        my_label:
            <m>()
            0x00 0x00 revert
    }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let macro_definition = parser.parse().unwrap().macros.get("WRAPPER").cloned().unwrap();
    let md_expected = MacroDefinition {
        name: "WRAPPER".to_string(),
        decorator: None,
        parameters: vec![Argument {
            arg_type: None,
            name: Some("m".to_string()),
            indexed: false,
            arg_location: None,
            span: AstSpan(vec![Span { start: 27, end: 28, file: None }]),
        }],
        statements: vec![Statement {
            ty: StatementType::Label(Label {
                name: "my_label".to_string(),
                inner: vec![
                    Statement {
                        ty: StatementType::ArgMacroInvocation("WRAPPER".to_string(), "m".to_string(), vec![]),
                        span: AstSpan(vec![Span { start: 84, end: 87, file: None }]),
                    },
                    Statement {
                        ty: StatementType::Literal(str_to_bytes32("00")),
                        span: AstSpan(vec![Span { start: 102, end: 106, file: None }]),
                    },
                    Statement {
                        ty: StatementType::Literal(str_to_bytes32("00")),
                        span: AstSpan(vec![Span { start: 107, end: 111, file: None }]),
                    },
                    Statement { ty: StatementType::Opcode(Opcode::Revert), span: AstSpan(vec![Span { start: 112, end: 118, file: None }]) },
                ],
                span: AstSpan(vec![
                    Span { start: 62, end: 70, file: None },
                    Span { start: 84, end: 87, file: None },
                    Span { start: 102, end: 106, file: None },
                    Span { start: 107, end: 111, file: None },
                    Span { start: 112, end: 118, file: None },
                ]),
            }),
            span: AstSpan(vec![
                Span { start: 62, end: 70, file: None },
                Span { start: 84, end: 87, file: None },
                Span { start: 102, end: 106, file: None },
                Span { start: 107, end: 111, file: None },
                Span { start: 112, end: 118, file: None },
            ]),
        }],
        takes: 0,
        returns: 0,
        span: AstSpan(vec![
            Span { start: 5, end: 12, file: None },
            Span { start: 13, end: 18, file: None },
            Span { start: 19, end: 26, file: None },
            Span { start: 26, end: 27, file: None },
            Span { start: 27, end: 28, file: None },
            Span { start: 28, end: 29, file: None },
            Span { start: 30, end: 31, file: None },
            Span { start: 32, end: 37, file: None },
            Span { start: 37, end: 38, file: None },
            Span { start: 38, end: 39, file: None },
            Span { start: 39, end: 40, file: None },
            Span { start: 41, end: 48, file: None },
            Span { start: 48, end: 49, file: None },
            Span { start: 49, end: 50, file: None },
            Span { start: 50, end: 51, file: None },
            Span { start: 52, end: 53, file: None },
            Span { start: 58, end: 66, file: None },
            Span { start: 66, end: 67, file: None },
            Span { start: 76, end: 77, file: None },
            Span { start: 77, end: 78, file: None },
            Span { start: 78, end: 79, file: None },
            Span { start: 79, end: 80, file: None },
            Span { start: 80, end: 81, file: None },
            Span { start: 94, end: 98, file: None },
            Span { start: 99, end: 103, file: None },
            Span { start: 104, end: 110, file: None },
            Span { start: 115, end: 116, file: None },
        ]),
        outlined: false,
        test: false,
    };
    assert_eq!(macro_definition.name, md_expected.name);
    assert_eq!(macro_definition.parameters, md_expected.parameters);
    assert_eq!(macro_definition.takes, md_expected.takes);
    assert_eq!(macro_definition.returns, md_expected.returns);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    // Test that each statement is the correct type
    for (i, s) in macro_definition.statements.iter().enumerate() {
        assert_eq!(s.ty, md_expected.statements[i].ty);
    }
}
