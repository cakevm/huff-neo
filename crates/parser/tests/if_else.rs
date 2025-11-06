use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::{file::full_file_source::FullFileSource, prelude::*};

#[test]
fn test_if_basic_literal_condition() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(0x01) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::IfStatement {
            condition: Expression::Literal { value: str_to_bytes32("01"), span: AstSpan(vec![Span { start: 48, end: 52, file: None }]) },
            then_branch: vec![],
            else_if_branches: vec![],
            else_branch: None,
        },
        span: AstSpan(vec![
            Span { start: 45, end: 47, file: None }, // "if"
            Span { start: 47, end: 48, file: None }, // "("
            Span { start: 48, end: 52, file: None }, // "0x01"
            Span { start: 52, end: 53, file: None }, // ")"
            Span { start: 54, end: 55, file: None }, // "{"
            Span { start: 56, end: 57, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_else_with_constant_condition() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([FLAG]) { } else { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::IfStatement {
            condition: Expression::Constant { name: "FLAG".to_string(), span: AstSpan(vec![Span { start: 48, end: 54, file: None }]) },
            then_branch: vec![],
            else_if_branches: vec![],
            else_branch: Some(vec![]),
        },
        span: AstSpan(vec![
            Span { start: 45, end: 47, file: None }, // "if"
            Span { start: 47, end: 48, file: None }, // "("
            Span { start: 48, end: 54, file: None }, // "[FLAG]"
            Span { start: 54, end: 55, file: None }, // ")"
            Span { start: 56, end: 57, file: None }, // "{"
            Span { start: 58, end: 59, file: None }, // "}"
            Span { start: 60, end: 64, file: None }, // "else"
            Span { start: 65, end: 66, file: None }, // "{"
            Span { start: 67, end: 68, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_comparison_equal() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([A] == [B]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::IfStatement {
            condition: Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "A".to_string(),
                    span: AstSpan(vec![Span { start: 48, end: 51, file: None }]),
                }),
                op: BinaryOp::Eq,
                right: Box::new(Expression::Constant {
                    name: "B".to_string(),
                    span: AstSpan(vec![Span { start: 55, end: 58, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 48, end: 51, file: None }, // "[A]"
                    Span { start: 52, end: 54, file: None }, // "=="
                    Span { start: 55, end: 58, file: None }, // "[B]"
                ]),
            },
            then_branch: vec![],
            else_if_branches: vec![],
            else_branch: None,
        },
        span: AstSpan(vec![
            Span { start: 45, end: 47, file: None }, // "if"
            Span { start: 47, end: 48, file: None }, // "("
            Span { start: 48, end: 51, file: None }, // "[A]"
            Span { start: 52, end: 54, file: None }, // "=="
            Span { start: 55, end: 58, file: None }, // "[B]"
            Span { start: 58, end: 59, file: None }, // ")"
            Span { start: 60, end: 61, file: None }, // "{"
            Span { start: 62, end: 63, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_comparison_greater_than() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([X] > [Y]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::IfStatement { condition, then_branch, else_if_branches, else_branch } => {
            match condition {
                Expression::Binary { op, .. } => {
                    assert_eq!(*op, BinaryOp::Gt);
                }
                _ => panic!("Expected binary expression"),
            }
            assert_eq!(then_branch.len(), 0);
            assert_eq!(else_if_branches.len(), 0);
            assert!(else_branch.is_none());
        }
        _ => panic!("Expected IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_logical_not() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(![FLAG]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::IfStatement { condition, .. } => match condition {
            Expression::Unary { op, .. } => {
                assert_eq!(*op, UnaryOp::Not);
            }
            _ => panic!("Expected unary expression"),
        },
        _ => panic!("Expected IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_arithmetic_in_condition() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([A] + [B]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::IfStatement { condition, .. } => match condition {
            Expression::Binary { op, .. } => {
                assert_eq!(*op, BinaryOp::Add);
            }
            _ => panic!("Expected binary expression"),
        },
        _ => panic!("Expected IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_nested() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([X]) { if([Y]) { } } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure for nested
    let outer_stmt = &macro_definition.statements[0];
    match &outer_stmt.ty {
        StatementType::IfStatement { condition, then_branch, else_if_branches, else_branch } => {
            assert!(matches!(condition, Expression::Constant { name, .. } if name == "X"));
            assert_eq!(then_branch.len(), 1);
            assert_eq!(else_if_branches.len(), 0);
            assert!(else_branch.is_none());
            match &then_branch[0].ty {
                StatementType::IfStatement { condition: inner_cond, .. } => {
                    assert!(matches!(inner_cond, Expression::Constant { name, .. } if name == "Y"));
                }
                _ => panic!("Expected nested IfStatement"),
            }
        }
        _ => panic!("Expected outer IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_body_statements() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(1) { 0x00 mstore } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::IfStatement { then_branch, .. } => {
            assert_eq!(then_branch.len(), 2);
            assert!(matches!(then_branch[0].ty, StatementType::Literal(_)));
            assert!(matches!(then_branch[1].ty, StatementType::Opcode(Opcode::Mstore)));
        }
        _ => panic!("Expected IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_else_if_chain() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([A]) { } else if([B]) { } else { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::IfStatement { condition, then_branch, else_if_branches, else_branch } => {
            assert!(matches!(condition, Expression::Constant { name, .. } if name == "A"));
            assert_eq!(then_branch.len(), 0);
            assert_eq!(else_if_branches.len(), 1);
            assert!(else_branch.is_some());

            // Check else if
            let (else_if_cond, else_if_body) = &else_if_branches[0];
            assert!(matches!(else_if_cond, Expression::Constant { name, .. } if name == "B"));
            assert_eq!(else_if_body.len(), 0);
        }
        _ => panic!("Expected IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_multiple_else_if() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([A]) { } else if([B]) { } else if([C]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure
    let stmt = &macro_definition.statements[0];
    match &stmt.ty {
        StatementType::IfStatement { else_if_branches, .. } => {
            assert_eq!(else_if_branches.len(), 2);
        }
        _ => panic!("Expected IfStatement"),
    }

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_sequential() {
    let source = "#define macro TEST() = takes(0) returns(0) { if([A]) { } if([B]) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    // Should parse as two separate if statements
    assert_eq!(macro_definition.statements.len(), 2);
    assert!(matches!(macro_definition.statements[0].ty, StatementType::IfStatement { .. }));
    assert!(matches!(macro_definition.statements[1].ty, StatementType::IfStatement { .. }));

    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_complex_condition() {
    let source = "#define macro TEST() = takes(0) returns(0) { if(([A] + [B]) > ([C] * 2)) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    // Check structure - just verify it parsed as IfStatement
    assert!(matches!(macro_definition.statements[0].ty, StatementType::IfStatement { .. }));
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_arg_in_condition() {
    let source = "#define macro TEST(threshold) = takes(0) returns(0) { if(<threshold>) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::IfStatement {
            condition: Expression::ArgCall {
                macro_name: String::new(),
                name: "threshold".to_string(),
                span: AstSpan(vec![Span { start: 57, end: 68, file: None }]), // "<threshold>"
            },
            then_branch: vec![],
            else_if_branches: vec![],
            else_branch: None,
        },
        span: AstSpan(vec![
            Span { start: 54, end: 56, file: None }, // "if"
            Span { start: 56, end: 57, file: None }, // "("
            Span { start: 57, end: 68, file: None }, // "<threshold>"
            Span { start: 68, end: 69, file: None }, // ")"
            Span { start: 70, end: 71, file: None }, // "{"
            Span { start: 72, end: 73, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_arg_arithmetic() {
    let source = "#define macro TEST(a, b) = takes(0) returns(0) { if(<a> + <b>) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::IfStatement {
            condition: Expression::Binary {
                left: Box::new(Expression::ArgCall {
                    macro_name: String::new(),
                    name: "a".to_string(),
                    span: AstSpan(vec![Span { start: 52, end: 55, file: None }]), // "<a>"
                }),
                op: BinaryOp::Add,
                right: Box::new(Expression::ArgCall {
                    macro_name: String::new(),
                    name: "b".to_string(),
                    span: AstSpan(vec![Span { start: 58, end: 61, file: None }]), // "<b>"
                }),
                span: AstSpan(vec![
                    Span { start: 52, end: 55, file: None }, // "<a>"
                    Span { start: 56, end: 57, file: None }, // "+"
                    Span { start: 58, end: 61, file: None }, // "<b>"
                ]),
            },
            then_branch: vec![],
            else_if_branches: vec![],
            else_branch: None,
        },
        span: AstSpan(vec![
            Span { start: 49, end: 51, file: None }, // "if"
            Span { start: 51, end: 52, file: None }, // "("
            Span { start: 52, end: 55, file: None }, // "<a>"
            Span { start: 56, end: 57, file: None }, // "+"
            Span { start: 58, end: 61, file: None }, // "<b>"
            Span { start: 61, end: 62, file: None }, // ")"
            Span { start: 63, end: 64, file: None }, // "{"
            Span { start: 65, end: 66, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}

#[test]
fn test_if_with_mixed_const_and_arg() {
    let source = "#define macro TEST(offset) = takes(0) returns(0) { if([BASE] + <offset> > 0x20) { } }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_definition = contract.macros.get("TEST").cloned().unwrap();

    assert_eq!(macro_definition.statements.len(), 1);

    let expected_statement = Statement {
        ty: StatementType::IfStatement {
            condition: Expression::Binary {
                left: Box::new(Expression::Binary {
                    left: Box::new(Expression::Constant {
                        name: "BASE".to_string(),
                        span: AstSpan(vec![Span { start: 54, end: 60, file: None }]),
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expression::ArgCall {
                        macro_name: String::new(),
                        name: "offset".to_string(),
                        span: AstSpan(vec![Span { start: 63, end: 71, file: None }]), // "<offset>"
                    }),
                    span: AstSpan(vec![
                        Span { start: 54, end: 60, file: None }, // "[BASE]"
                        Span { start: 61, end: 62, file: None }, // "+"
                        Span { start: 63, end: 71, file: None }, // "<offset>"
                    ]),
                }),
                op: BinaryOp::Gt,
                right: Box::new(Expression::Literal {
                    value: str_to_bytes32("20"),
                    span: AstSpan(vec![Span { start: 74, end: 78, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 54, end: 60, file: None }, // "[BASE]"
                    Span { start: 61, end: 62, file: None }, // "+"
                    Span { start: 63, end: 71, file: None }, // "<offset>"
                    Span { start: 72, end: 73, file: None }, // ">"
                    Span { start: 74, end: 78, file: None }, // "0x20"
                ]),
            },
            then_branch: vec![],
            else_if_branches: vec![],
            else_branch: None,
        },
        span: AstSpan(vec![
            Span { start: 51, end: 53, file: None }, // "if"
            Span { start: 53, end: 54, file: None }, // "("
            Span { start: 54, end: 60, file: None }, // "[BASE]"
            Span { start: 61, end: 62, file: None }, // "+"
            Span { start: 63, end: 71, file: None }, // "<offset>"
            Span { start: 72, end: 73, file: None }, // ">"
            Span { start: 74, end: 78, file: None }, // "0x20"
            Span { start: 78, end: 79, file: None }, // ")"
            Span { start: 80, end: 81, file: None }, // "{"
            Span { start: 82, end: 83, file: None }, // "}"
        ]),
    };

    assert_eq!(macro_definition.statements[0], expected_statement);
    assert_eq!(parser.current_token.kind, TokenKind::Eof);
}
