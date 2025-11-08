use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::{file::full_file_source::FullFileSource, prelude::*};

#[test]
fn test_for_loop_basic_literals() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..5) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 54, end: 55, file: None }]) },
            end: Expression::Literal { value: str_to_bytes32("05"), span: AstSpan(vec![Span { start: 57, end: 58, file: None }]) },
            step: None,
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 45, end: 48, file: None }, // "for"
            Span { start: 48, end: 49, file: None }, // "("
            Span { start: 49, end: 50, file: None }, // "i"
            Span { start: 51, end: 53, file: None }, // "in"
            Span { start: 54, end: 55, file: None }, // "0"
            Span { start: 55, end: 57, file: None }, // ".."
            Span { start: 57, end: 58, file: None }, // "5"
            Span { start: 58, end: 59, file: None }, // ")"
            Span { start: 60, end: 61, file: None }, // "{"
            Span { start: 62, end: 63, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_step() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..10 step 2) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 54, end: 55, file: None }]) },
            end: Expression::Literal { value: str_to_bytes32("0a"), span: AstSpan(vec![Span { start: 57, end: 59, file: None }]) },
            step: Some(Expression::Literal { value: str_to_bytes32("02"), span: AstSpan(vec![Span { start: 65, end: 66, file: None }]) }),
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 45, end: 48, file: None }, // "for"
            Span { start: 48, end: 49, file: None }, // "("
            Span { start: 49, end: 50, file: None }, // "i"
            Span { start: 51, end: 53, file: None }, // "in"
            Span { start: 54, end: 55, file: None }, // "0"
            Span { start: 55, end: 57, file: None }, // ".."
            Span { start: 57, end: 59, file: None }, // "10"
            Span { start: 60, end: 64, file: None }, // "step"
            Span { start: 65, end: 66, file: None }, // "2"
            Span { start: 66, end: 67, file: None }, // ")"
            Span { start: 68, end: 69, file: None }, // "{"
            Span { start: 70, end: 71, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_constants() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in [START]..[END]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Constant { name: "START".to_string(), span: AstSpan(vec![Span { start: 54, end: 61, file: None }]) },
            end: Expression::Constant { name: "END".to_string(), span: AstSpan(vec![Span { start: 63, end: 68, file: None }]) },
            step: None,
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 45, end: 48, file: None }, // "for"
            Span { start: 48, end: 49, file: None }, // "("
            Span { start: 49, end: 50, file: None }, // "i"
            Span { start: 51, end: 53, file: None }, // "in"
            Span { start: 54, end: 61, file: None }, // "[START]"
            Span { start: 61, end: 63, file: None }, // ".."
            Span { start: 63, end: 68, file: None }, // "[END]"
            Span { start: 68, end: 69, file: None }, // ")"
            Span { start: 70, end: 71, file: None }, // "{"
            Span { start: 72, end: 73, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_arithmetic_bounds() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..[SIZE] * 2) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 54, end: 55, file: None }]) },
            end: Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "SIZE".to_string(),
                    span: AstSpan(vec![Span { start: 57, end: 63, file: None }]),
                }),
                op: BinaryOp::Mul,
                right: Box::new(Expression::Literal {
                    value: str_to_bytes32("02"),
                    span: AstSpan(vec![Span { start: 66, end: 67, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 57, end: 63, file: None }, // "[SIZE]"
                    Span { start: 64, end: 65, file: None }, // "*"
                    Span { start: 66, end: 67, file: None }, // "2"
                ]),
            },
            step: None,
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 45, end: 48, file: None }, // "for"
            Span { start: 48, end: 49, file: None }, // "("
            Span { start: 49, end: 50, file: None }, // "i"
            Span { start: 51, end: 53, file: None }, // "in"
            Span { start: 54, end: 55, file: None }, // "0"
            Span { start: 55, end: 57, file: None }, // ".."
            Span { start: 57, end: 63, file: None }, // "[SIZE]"
            Span { start: 64, end: 65, file: None }, // "*"
            Span { start: 66, end: 67, file: None }, // "2"
            Span { start: 67, end: 68, file: None }, // ")"
            Span { start: 69, end: 70, file: None }, // "{"
            Span { start: 71, end: 72, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_variable_interpolation() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..3) { <i> } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Just check the structure, not exact spans since body parsing may differ
    let actual_stmt = &macro_definition.statements[0];
    match &actual_stmt.ty {
        StatementType::ForLoop { variable, start, end, step, body } => {
            assert_eq!(variable, "i");
            assert!(matches!(start, Expression::Literal { .. }));
            assert!(matches!(end, Expression::Literal { .. }));
            assert!(step.is_none());
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].ty, StatementType::Constant(ref s) if s == "__LOOP_VAR_i"));
        }
        _ => panic!("Expected ForLoop statement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_nested() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..2) { for(j in 0..2) { } } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Just check structure for nested loops due to complexity
    let outer_stmt = &macro_definition.statements[0];
    match &outer_stmt.ty {
        StatementType::ForLoop { variable, body, .. } => {
            assert_eq!(variable, "i");
            assert_eq!(body.len(), 1);
            match &body[0].ty {
                StatementType::ForLoop { variable: inner_var, .. } => {
                    assert_eq!(inner_var, "j");
                }
                _ => panic!("Expected nested ForLoop"),
            }
        }
        _ => panic!("Expected outer ForLoop"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_sequential() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..2) { } for(j in 0..3) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 2);

    // Check first loop
    match &macro_definition.statements[0].ty {
        StatementType::ForLoop { variable, .. } => {
            assert_eq!(variable, "i");
        }
        _ => panic!("Expected first ForLoop"),
    }

    // Check second loop
    match &macro_definition.statements[1].ty {
        StatementType::ForLoop { variable, .. } => {
            assert_eq!(variable, "j");
        }
        _ => panic!("Expected second ForLoop"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_hex_bounds() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0x00..0xFF) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 54, end: 58, file: None }]) },
            end: Expression::Literal { value: str_to_bytes32("ff"), span: AstSpan(vec![Span { start: 60, end: 64, file: None }]) },
            step: None,
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 45, end: 48, file: None }, // "for"
            Span { start: 48, end: 49, file: None }, // "("
            Span { start: 49, end: 50, file: None }, // "i"
            Span { start: 51, end: 53, file: None }, // "in"
            Span { start: 54, end: 58, file: None }, // "0x00"
            Span { start: 58, end: 60, file: None }, // ".."
            Span { start: 60, end: 64, file: None }, // "0xFF"
            Span { start: 64, end: 65, file: None }, // ")"
            Span { start: 66, end: 67, file: None }, // "{"
            Span { start: 68, end: 69, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_body_statements() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..3) { <i> 0x00 mstore } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure rather than exact spans due to body complexity
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::ForLoop { variable, body, .. } => {
            assert_eq!(variable, "i");
            assert_eq!(body.len(), 3);
            assert!(matches!(body[0].ty, StatementType::Constant(ref s) if s == "__LOOP_VAR_i"));
            assert!(matches!(body[1].ty, StatementType::Literal(_)));
            assert!(matches!(body[2].ty, StatementType::Opcode(Opcode::Mstore)));
        }
        _ => panic!("Expected ForLoop"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_constant_step() {
    let source = "#define macro TEST() = takes(0) returns(0) { for(i in 0..10 step [STEP]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 54, end: 55, file: None }]) },
            end: Expression::Literal { value: str_to_bytes32("0a"), span: AstSpan(vec![Span { start: 57, end: 59, file: None }]) },
            step: Some(Expression::Constant { name: "STEP".to_string(), span: AstSpan(vec![Span { start: 65, end: 71, file: None }]) }),
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 45, end: 48, file: None }, // "for"
            Span { start: 48, end: 49, file: None }, // "("
            Span { start: 49, end: 50, file: None }, // "i"
            Span { start: 51, end: 53, file: None }, // "in"
            Span { start: 54, end: 55, file: None }, // "0"
            Span { start: 55, end: 57, file: None }, // ".."
            Span { start: 57, end: 59, file: None }, // "10"
            Span { start: 60, end: 64, file: None }, // "step"
            Span { start: 65, end: 71, file: None }, // "[STEP]"
            Span { start: 71, end: 72, file: None }, // ")"
            Span { start: 73, end: 74, file: None }, // "{"
            Span { start: 75, end: 76, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_arg_in_bound() {
    let source = "#define macro TEST(count) = takes(0) returns(0) { for(i in 0..<count>) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 59, end: 60, file: None }]) },
            end: Expression::ArgCall {
                macro_name: String::new(),
                name: "count".to_string(),
                span: AstSpan(vec![Span { start: 62, end: 69, file: None }]), // "<count>"
            },
            step: None,
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 50, end: 53, file: None }, // "for"
            Span { start: 53, end: 54, file: None }, // "("
            Span { start: 54, end: 55, file: None }, // "i"
            Span { start: 56, end: 58, file: None }, // "in"
            Span { start: 59, end: 60, file: None }, // "0"
            Span { start: 60, end: 62, file: None }, // ".."
            Span { start: 62, end: 69, file: None }, // "<count>"
            Span { start: 69, end: 70, file: None }, // ")"
            Span { start: 71, end: 72, file: None }, // "{"
            Span { start: 73, end: 74, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_arg_arithmetic_bound() {
    let source = "#define macro TEST(base, extra) = takes(0) returns(0) { for(i in 0..(<base> + <extra>)) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 65, end: 66, file: None }]) },
            end: Expression::Grouped {
                expr: Box::new(Expression::Binary {
                    left: Box::new(Expression::ArgCall {
                        macro_name: String::new(),
                        name: "base".to_string(),
                        span: AstSpan(vec![Span { start: 69, end: 75, file: None }]), // "<base>"
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expression::ArgCall {
                        macro_name: String::new(),
                        name: "extra".to_string(),
                        span: AstSpan(vec![Span { start: 78, end: 85, file: None }]), // "<extra>"
                    }),
                    span: AstSpan(vec![
                        Span { start: 69, end: 75, file: None }, // "<base>"
                        Span { start: 76, end: 77, file: None }, // "+"
                        Span { start: 78, end: 85, file: None }, // "<extra>"
                    ]),
                }),
                span: AstSpan(vec![
                    Span { start: 68, end: 69, file: None }, // "("
                    Span { start: 85, end: 86, file: None }, // ")"
                ]),
            },
            step: None,
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 56, end: 59, file: None }, // "for"
            Span { start: 59, end: 60, file: None }, // "("
            Span { start: 60, end: 61, file: None }, // "i"
            Span { start: 62, end: 64, file: None }, // "in"
            Span { start: 65, end: 66, file: None }, // "0"
            Span { start: 66, end: 68, file: None }, // ".."
            Span { start: 68, end: 69, file: None }, // "("
            Span { start: 85, end: 86, file: None }, // ")"
            Span { start: 86, end: 87, file: None }, // ")"
            Span { start: 88, end: 89, file: None }, // "{"
            Span { start: 90, end: 91, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_for_loop_with_arg_in_step() {
    let source = "#define macro TEST(step_size) = takes(0) returns(0) { for(i in 0..10 step <step_size>) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::ForLoop {
            variable: "i".to_string(),
            start: Expression::Literal { value: str_to_bytes32("00"), span: AstSpan(vec![Span { start: 63, end: 64, file: None }]) },
            end: Expression::Literal { value: str_to_bytes32("0a"), span: AstSpan(vec![Span { start: 66, end: 68, file: None }]) },
            step: Some(Expression::ArgCall {
                macro_name: String::new(),
                name: "step_size".to_string(),
                span: AstSpan(vec![Span { start: 74, end: 85, file: None }]), // "<step_size>"
            }),
            body: vec![],
        },
        span: AstSpan(vec![
            Span { start: 54, end: 57, file: None }, // "for"
            Span { start: 57, end: 58, file: None }, // "("
            Span { start: 58, end: 59, file: None }, // "i"
            Span { start: 60, end: 62, file: None }, // "in"
            Span { start: 63, end: 64, file: None }, // "0"
            Span { start: 64, end: 66, file: None }, // ".."
            Span { start: 66, end: 68, file: None }, // "10"
            Span { start: 69, end: 73, file: None }, // "step"
            Span { start: 74, end: 85, file: None }, // "<step_size>"
            Span { start: 85, end: 86, file: None }, // ")"
            Span { start: 87, end: 88, file: None }, // "{"
            Span { start: 89, end: 90, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}
