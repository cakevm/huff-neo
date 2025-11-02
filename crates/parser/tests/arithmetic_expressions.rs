use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

// Basic Arithmetic Operations

#[test]
fn test_parse_simple_addition() {
    let source = "#define constant RESULT = [A] + [B]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Add,
            right: Box::new(Expression::Constant { name: "B".to_string(), span: AstSpan(vec![Span { start: 32, end: 35, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "+"
                Span { start: 32, end: 35, file: None }, // "[B]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "+"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_simple_subtraction() {
    let source = "#define constant RESULT = [A] - [B]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Sub,
            right: Box::new(Expression::Constant { name: "B".to_string(), span: AstSpan(vec![Span { start: 32, end: 35, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "-"
                Span { start: 32, end: 35, file: None }, // "[B]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "-"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_simple_multiplication() {
    let source = "#define constant RESULT = [A] * [B]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Mul,
            right: Box::new(Expression::Constant { name: "B".to_string(), span: AstSpan(vec![Span { start: 32, end: 35, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None },
                Span { start: 30, end: 31, file: None },
                Span { start: 32, end: 35, file: None },
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },
            Span { start: 8, end: 16, file: None },
            Span { start: 17, end: 23, file: None },
            Span { start: 24, end: 25, file: None },
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None },
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_simple_division() {
    let source = "#define constant RESULT = [A] / [B]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Div,
            right: Box::new(Expression::Constant { name: "B".to_string(), span: AstSpan(vec![Span { start: 32, end: 35, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None },
                Span { start: 30, end: 31, file: None },
                Span { start: 32, end: 35, file: None },
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },
            Span { start: 8, end: 16, file: None },
            Span { start: 17, end: 23, file: None },
            Span { start: 24, end: 25, file: None },
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None },
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_simple_modulo() {
    let source = "#define constant RESULT = [A] % [B]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Mod,
            right: Box::new(Expression::Constant { name: "B".to_string(), span: AstSpan(vec![Span { start: 32, end: 35, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None },
                Span { start: 30, end: 31, file: None },
                Span { start: 32, end: 35, file: None },
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },
            Span { start: 8, end: 16, file: None },
            Span { start: 17, end: 23, file: None },
            Span { start: 24, end: 25, file: None },
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "%"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_unary_negation() {
    let source = "#define constant RESULT = -[A]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 27, end: 30, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 27, file: None }, // "-"
                Span { start: 27, end: 30, file: None }, // "[A]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },
            Span { start: 8, end: 16, file: None },
            Span { start: 17, end: 23, file: None },
            Span { start: 24, end: 25, file: None },
            Span { start: 26, end: 27, file: None }, // "-"
            Span { start: 27, end: 28, file: None }, // "["
            Span { start: 28, end: 29, file: None }, // "A"
            Span { start: 29, end: 30, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

// Operator Precedence

#[test]
fn test_parse_precedence_mul_before_add() {
    let source = "#define constant RESULT = [A] + [B] * [C]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();

    // Should parse as: A + (B * C)
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Add,
            right: Box::new(Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "B".to_string(),
                    span: AstSpan(vec![Span { start: 32, end: 35, file: None }]),
                }),
                op: BinaryOp::Mul,
                right: Box::new(Expression::Constant {
                    name: "C".to_string(),
                    span: AstSpan(vec![Span { start: 38, end: 41, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 32, end: 35, file: None }, // "[B]"
                    Span { start: 36, end: 37, file: None }, // "*"
                    Span { start: 38, end: 41, file: None }, // "[C]"
                ]),
            }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "+"
                Span { start: 32, end: 35, file: None }, // "[B]"
                Span { start: 36, end: 37, file: None }, // "*"
                Span { start: 38, end: 41, file: None }, // "[C]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "+"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
            Span { start: 36, end: 37, file: None }, // "*"
            Span { start: 38, end: 39, file: None }, // "["
            Span { start: 39, end: 40, file: None }, // "C"
            Span { start: 40, end: 41, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_precedence_div_before_sub() {
    let source = "#define constant RESULT = [A] - [B] / [C]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();

    // Should parse as: A - (B / C)
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Sub,
            right: Box::new(Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "B".to_string(),
                    span: AstSpan(vec![Span { start: 32, end: 35, file: None }]),
                }),
                op: BinaryOp::Div,
                right: Box::new(Expression::Constant {
                    name: "C".to_string(),
                    span: AstSpan(vec![Span { start: 38, end: 41, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 32, end: 35, file: None }, // "[B]"
                    Span { start: 36, end: 37, file: None }, // "/"
                    Span { start: 38, end: 41, file: None }, // "[C]"
                ]),
            }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "-"
                Span { start: 32, end: 35, file: None }, // "[B]"
                Span { start: 36, end: 37, file: None }, // "/"
                Span { start: 38, end: 41, file: None }, // "[C]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "-"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
            Span { start: 36, end: 37, file: None }, // "/"
            Span { start: 38, end: 39, file: None }, // "["
            Span { start: 39, end: 40, file: None }, // "C"
            Span { start: 40, end: 41, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_precedence_mod_before_add() {
    let source = "#define constant RESULT = [A] + [B] % [C]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();

    // Should parse as: A + (B % C)
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "A".to_string(), span: AstSpan(vec![Span { start: 26, end: 29, file: None }]) }),
            op: BinaryOp::Add,
            right: Box::new(Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "B".to_string(),
                    span: AstSpan(vec![Span { start: 32, end: 35, file: None }]),
                }),
                op: BinaryOp::Mod,
                right: Box::new(Expression::Constant {
                    name: "C".to_string(),
                    span: AstSpan(vec![Span { start: 38, end: 41, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 32, end: 35, file: None }, // "[B]"
                    Span { start: 36, end: 37, file: None }, // "%"
                    Span { start: 38, end: 41, file: None }, // "[C]"
                ]),
            }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "+"
                Span { start: 32, end: 35, file: None }, // "[B]"
                Span { start: 36, end: 37, file: None }, // "%"
                Span { start: 38, end: 41, file: None }, // "[C]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "+"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
            Span { start: 36, end: 37, file: None }, // "%"
            Span { start: 38, end: 39, file: None }, // "["
            Span { start: 39, end: 40, file: None }, // "C"
            Span { start: 40, end: 41, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_precedence_same_level_left_assoc() {
    let source = "#define constant RESULT = [A] - [B] + [C]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();

    // Should parse as: (A - B) + C (left-associative)
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "A".to_string(),
                    span: AstSpan(vec![Span { start: 26, end: 29, file: None }]),
                }),
                op: BinaryOp::Sub,
                right: Box::new(Expression::Constant {
                    name: "B".to_string(),
                    span: AstSpan(vec![Span { start: 32, end: 35, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 26, end: 29, file: None }, // "[A]"
                    Span { start: 30, end: 31, file: None }, // "-"
                    Span { start: 32, end: 35, file: None }, // "[B]"
                ]),
            }),
            op: BinaryOp::Add,
            right: Box::new(Expression::Constant { name: "C".to_string(), span: AstSpan(vec![Span { start: 38, end: 41, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "-"
                Span { start: 32, end: 35, file: None }, // "[B]"
                Span { start: 36, end: 37, file: None }, // "+"
                Span { start: 38, end: 41, file: None }, // "[C]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "-"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
            Span { start: 36, end: 37, file: None }, // "+"
            Span { start: 38, end: 39, file: None }, // "["
            Span { start: 39, end: 40, file: None }, // "C"
            Span { start: 40, end: 41, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_precedence_mul_div_left_assoc() {
    let source = "#define constant RESULT = [A] * [B] / [C]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();

    // Should parse as: (A * B) / C (left-associative)
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Binary {
                left: Box::new(Expression::Constant {
                    name: "A".to_string(),
                    span: AstSpan(vec![Span { start: 26, end: 29, file: None }]),
                }),
                op: BinaryOp::Mul,
                right: Box::new(Expression::Constant {
                    name: "B".to_string(),
                    span: AstSpan(vec![Span { start: 32, end: 35, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 26, end: 29, file: None }, // "[A]"
                    Span { start: 30, end: 31, file: None }, // "*"
                    Span { start: 32, end: 35, file: None }, // "[B]"
                ]),
            }),
            op: BinaryOp::Div,
            right: Box::new(Expression::Constant { name: "C".to_string(), span: AstSpan(vec![Span { start: 38, end: 41, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 29, file: None }, // "[A]"
                Span { start: 30, end: 31, file: None }, // "*"
                Span { start: 32, end: 35, file: None }, // "[B]"
                Span { start: 36, end: 37, file: None }, // "/"
                Span { start: 38, end: 41, file: None }, // "[C]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 28, file: None }, // "A"
            Span { start: 28, end: 29, file: None }, // "]"
            Span { start: 30, end: 31, file: None }, // "*"
            Span { start: 32, end: 33, file: None }, // "["
            Span { start: 33, end: 34, file: None }, // "B"
            Span { start: 34, end: 35, file: None }, // "]"
            Span { start: 36, end: 37, file: None }, // "/"
            Span { start: 38, end: 39, file: None }, // "["
            Span { start: 39, end: 40, file: None }, // "C"
            Span { start: 40, end: 41, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

// Grouping and Parentheses

#[test]
fn test_parse_grouped_expression() {
    let source = "#define constant RESULT = ([A] + [B]) * [C]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();

    // Should parse as: (A + B) * C
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Grouped {
                expr: Box::new(Expression::Binary {
                    left: Box::new(Expression::Constant {
                        name: "A".to_string(),
                        span: AstSpan(vec![Span { start: 27, end: 30, file: None }]),
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expression::Constant {
                        name: "B".to_string(),
                        span: AstSpan(vec![Span { start: 33, end: 36, file: None }]),
                    }),
                    span: AstSpan(vec![
                        Span { start: 27, end: 30, file: None }, // "[A]"
                        Span { start: 31, end: 32, file: None }, // "+"
                        Span { start: 33, end: 36, file: None }, // "[B]"
                    ]),
                }),
                span: AstSpan(vec![
                    Span { start: 26, end: 27, file: None }, // "("
                    Span { start: 36, end: 37, file: None }, // ")"
                ]),
            }),
            op: BinaryOp::Mul,
            right: Box::new(Expression::Constant { name: "C".to_string(), span: AstSpan(vec![Span { start: 40, end: 43, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 27, file: None }, // "("
                Span { start: 36, end: 37, file: None }, // ")"
                Span { start: 38, end: 39, file: None }, // "*"
                Span { start: 40, end: 43, file: None }, // "[C]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "("
            Span { start: 27, end: 28, file: None }, // "["
            Span { start: 28, end: 29, file: None }, // "A"
            Span { start: 29, end: 30, file: None }, // "]"
            Span { start: 31, end: 32, file: None }, // "+"
            Span { start: 33, end: 34, file: None }, // "["
            Span { start: 34, end: 35, file: None }, // "B"
            Span { start: 35, end: 36, file: None }, // "]"
            Span { start: 36, end: 37, file: None }, // ")"
            Span { start: 38, end: 39, file: None }, // "*"
            Span { start: 40, end: 41, file: None }, // "["
            Span { start: 41, end: 42, file: None }, // "C"
            Span { start: 42, end: 43, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_nested_parentheses() {
    let source = "#define constant RESULT = (([A] + [B]) * ([C] + [D]))";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    // Verify it's a grouped expression containing multiplication
    match constant.value {
        ConstVal::Expression(Expression::Grouped { expr: inner, .. }) => {
            match *inner {
                Expression::Binary { left, op, right, .. } => {
                    assert_eq!(op, BinaryOp::Mul);
                    // Verify left is grouped (A + B)
                    assert!(matches!(*left, Expression::Grouped { .. }));
                    // Verify right is grouped (C + D)
                    assert!(matches!(*right, Expression::Grouped { .. }));
                }
                _ => panic!("Expected Binary inside outer Grouped"),
            }
        }
        _ => panic!("Expected Grouped expression"),
    }
}

#[test]
fn test_parse_deeply_nested_parentheses() {
    let source = "#define constant RESULT = (((([A]))))";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    // Verify it's nested grouped expressions
    match constant.value {
        ConstVal::Expression(Expression::Grouped { expr: inner1, .. }) => match *inner1 {
            Expression::Grouped { expr: inner2, .. } => match *inner2 {
                Expression::Grouped { expr: inner3, .. } => match *inner3 {
                    Expression::Grouped { expr: inner4, .. } => {
                        assert!(matches!(*inner4, Expression::Constant { name, .. } if name == "A"));
                    }
                    _ => panic!("Expected 4th level Grouped"),
                },
                _ => panic!("Expected 3rd level Grouped"),
            },
            _ => panic!("Expected 2nd level Grouped"),
        },
        _ => panic!("Expected Grouped expression"),
    }
}

#[test]
fn test_parse_negation_of_grouped() {
    let source = "#define constant RESULT = -([A] + [B])";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(Expression::Grouped {
                expr: Box::new(Expression::Binary {
                    left: Box::new(Expression::Constant {
                        name: "A".to_string(),
                        span: AstSpan(vec![Span { start: 28, end: 31, file: None }]),
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expression::Constant {
                        name: "B".to_string(),
                        span: AstSpan(vec![Span { start: 34, end: 37, file: None }]),
                    }),
                    span: AstSpan(vec![
                        Span { start: 28, end: 31, file: None }, // "[A]"
                        Span { start: 32, end: 33, file: None }, // "+"
                        Span { start: 34, end: 37, file: None }, // "[B]"
                    ]),
                }),
                span: AstSpan(vec![
                    Span { start: 27, end: 28, file: None }, // "("
                    Span { start: 37, end: 38, file: None }, // ")"
                ]),
            }),
            span: AstSpan(vec![
                Span { start: 26, end: 27, file: None }, // "-"
                Span { start: 37, end: 38, file: None }, // last span from grouped expr
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "-"
            Span { start: 27, end: 28, file: None }, // "("
            Span { start: 28, end: 29, file: None }, // "["
            Span { start: 29, end: 30, file: None }, // "A"
            Span { start: 30, end: 31, file: None }, // "]"
            Span { start: 32, end: 33, file: None }, // "+"
            Span { start: 34, end: 35, file: None }, // "["
            Span { start: 35, end: 36, file: None }, // "B"
            Span { start: 36, end: 37, file: None }, // "]"
            Span { start: 37, end: 38, file: None }, // ")"
        ]),
    };
    assert_eq!(constant, expected);
}

// Mixed Expressions

#[test]
fn test_parse_hex_literal_in_expression() {
    let source = "#define constant RESULT = 0x10 + 0x05";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    match constant.value {
        ConstVal::Expression(Expression::Binary { left, op, right, .. }) => {
            assert_eq!(op, BinaryOp::Add);
            assert!(matches!(*left, Expression::Literal { .. }));
            assert!(matches!(*right, Expression::Literal { .. }));
        }
        _ => panic!("Expected Binary expression"),
    }
}

#[test]
fn test_parse_constant_reference() {
    let source = "#define constant RESULT = [BASE] + [OFFSET]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Constant { name: "BASE".to_string(), span: AstSpan(vec![Span { start: 26, end: 32, file: None }]) }),
            op: BinaryOp::Add,
            right: Box::new(Expression::Constant {
                name: "OFFSET".to_string(),
                span: AstSpan(vec![Span { start: 35, end: 43, file: None }]),
            }),
            span: AstSpan(vec![
                Span { start: 26, end: 32, file: None },
                Span { start: 33, end: 34, file: None },
                Span { start: 35, end: 43, file: None },
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },
            Span { start: 8, end: 16, file: None },
            Span { start: 17, end: 23, file: None },
            Span { start: 24, end: 25, file: None },
            Span { start: 26, end: 27, file: None }, // "["
            Span { start: 27, end: 31, file: None }, // "BASE"
            Span { start: 31, end: 32, file: None }, // "]"
            Span { start: 33, end: 34, file: None }, // "+"
            Span { start: 35, end: 36, file: None }, // "["
            Span { start: 36, end: 42, file: None }, // "OFFSET"
            Span { start: 42, end: 43, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_complex_nested() {
    let source = "#define constant RESULT = ([A] + [B]) * [C] - [D] / [E]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    // Should parse as: ((A + B) * C) - (D / E)
    match constant.value {
        ConstVal::Expression(Expression::Binary { left, op, right, .. }) => {
            assert_eq!(op, BinaryOp::Sub);

            // Left: (A + B) * C
            match *left {
                Expression::Binary { left: mul_left, op: mul_op, right: mul_right, .. } => {
                    assert_eq!(mul_op, BinaryOp::Mul);
                    assert!(matches!(*mul_left, Expression::Grouped { .. }));
                    assert!(matches!(*mul_right, Expression::Constant { name, .. } if name == "C"));
                }
                _ => panic!("Expected multiplication on left"),
            }

            // Right: D / E
            match *right {
                Expression::Binary { left: div_left, op: div_op, right: div_right, .. } => {
                    assert_eq!(div_op, BinaryOp::Div);
                    assert!(matches!(*div_left, Expression::Constant { name, .. } if name == "D"));
                    assert!(matches!(*div_right, Expression::Constant { name, .. } if name == "E"));
                }
                _ => panic!("Expected division on right"),
            }
        }
        _ => panic!("Expected Binary expression"),
    }
}

#[test]
fn test_parse_all_operators() {
    let source = "#define constant RESULT = (([A] + [B]) * [C] - [D]) / [E] % [F]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    // Verify it parses without error and top level has modulo (last operation)
    match constant.value {
        ConstVal::Expression(Expression::Binary { op, .. }) => {
            assert_eq!(op, BinaryOp::Mod);
        }
        _ => panic!("Expected Binary expression"),
    }
}

// Edge Cases

#[test]
fn test_parse_double_negation() {
    let source = "#define constant RESULT = --[A]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    match constant.value {
        ConstVal::Expression(Expression::Unary { op: op1, expr: inner1, .. }) => {
            assert_eq!(op1, UnaryOp::Neg);
            match *inner1 {
                Expression::Unary { op: op2, expr: inner2, .. } => {
                    assert_eq!(op2, UnaryOp::Neg);
                    assert!(matches!(*inner2, Expression::Constant { name, .. } if name == "A"));
                }
                _ => panic!("Expected second negation"),
            }
        }
        _ => panic!("Expected Unary expression"),
    }
}

#[test]
fn test_parse_negation_in_expression() {
    let source = "#define constant RESULT = -[A] + [B]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Expression(Expression::Binary {
            left: Box::new(Expression::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(Expression::Constant {
                    name: "A".to_string(),
                    span: AstSpan(vec![Span { start: 27, end: 30, file: None }]),
                }),
                span: AstSpan(vec![
                    Span { start: 26, end: 27, file: None }, // "-"
                    Span { start: 27, end: 30, file: None }, // "[A]"
                ]),
            }),
            op: BinaryOp::Add,
            right: Box::new(Expression::Constant { name: "B".to_string(), span: AstSpan(vec![Span { start: 33, end: 36, file: None }]) }),
            span: AstSpan(vec![
                Span { start: 26, end: 27, file: None }, // "-"
                Span { start: 27, end: 30, file: None }, // "[A]"
                Span { start: 31, end: 32, file: None }, // "+"
                Span { start: 33, end: 36, file: None }, // "[B]"
            ]),
        }),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },   // "#define"
            Span { start: 8, end: 16, file: None },  // "constant"
            Span { start: 17, end: 23, file: None }, // "RESULT"
            Span { start: 24, end: 25, file: None }, // "="
            Span { start: 26, end: 27, file: None }, // "-"
            Span { start: 27, end: 28, file: None }, // "["
            Span { start: 28, end: 29, file: None }, // "A"
            Span { start: 29, end: 30, file: None }, // "]"
            Span { start: 31, end: 32, file: None }, // "+"
            Span { start: 33, end: 34, file: None }, // "["
            Span { start: 34, end: 35, file: None }, // "B"
            Span { start: 35, end: 36, file: None }, // "]"
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_chained_operations() {
    let source = "#define constant RESULT = [A] + [B] + [C] + [D]";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    // Should parse as: (((A + B) + C) + D) (left-associative)
    match constant.value {
        ConstVal::Expression(Expression::Binary { left: level1_left, op: level1_op, right: level1_right, .. }) => {
            assert_eq!(level1_op, BinaryOp::Add);
            assert!(matches!(*level1_right, Expression::Constant { name, .. } if name == "D"));

            match *level1_left {
                Expression::Binary { left: level2_left, op: level2_op, right: level2_right, .. } => {
                    assert_eq!(level2_op, BinaryOp::Add);
                    assert!(matches!(*level2_right, Expression::Constant { name, .. } if name == "C"));

                    match *level2_left {
                        Expression::Binary { left: level3_left, op: level3_op, right: level3_right, .. } => {
                            assert_eq!(level3_op, BinaryOp::Add);
                            assert!(matches!(*level3_left, Expression::Constant { name, .. } if name == "A"));
                            assert!(matches!(*level3_right, Expression::Constant { name, .. } if name == "B"));
                        }
                        _ => panic!("Expected third level addition"),
                    }
                }
                _ => panic!("Expected second level addition"),
            }
        }
        _ => panic!("Expected Binary expression"),
    }
}

// Literals

#[test]
fn test_parse_literal_only() {
    let source = "#define constant RESULT = 0x10";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    let expected = ConstantDefinition {
        name: "RESULT".to_string(),
        value: ConstVal::Bytes(Bytes("10".to_string())),
        span: AstSpan(vec![
            Span { start: 0, end: 7, file: None },
            Span { start: 8, end: 16, file: None },
            Span { start: 17, end: 23, file: None },
            Span { start: 24, end: 25, file: None },
            Span { start: 26, end: 30, file: None },
        ]),
    };
    assert_eq!(constant, expected);
}

#[test]
fn test_parse_large_literal() {
    let source = "#define constant RESULT = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff + 0x00";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    assert_eq!(parser.current_token.kind, TokenKind::Eof);

    let constant = contract.constants.lock().unwrap()[0].clone();
    assert_eq!(constant.name, "RESULT");

    match constant.value {
        ConstVal::Expression(Expression::Binary { left, op, right, .. }) => {
            assert_eq!(op, BinaryOp::Add);
            assert!(matches!(*left, Expression::Literal { .. }));
            assert!(matches!(*right, Expression::Literal { .. }));
        }
        _ => panic!("Expected Binary expression"),
    }
}
