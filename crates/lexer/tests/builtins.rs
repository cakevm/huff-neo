use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn parses_builtin_function_in_macro_body() {
    let builtin_funcs = [
        "__codesize",
        "__tablesize",
        "__tablestart",
        "__FUNC_SIG",
        "__EVENT_HASH",
        "__ERROR",
        "__RIGHTPAD",
        "__CODECOPY_DYN_ARG",
        "__ASSERT_PC",
    ];

    for builtin in builtin_funcs {
        let source = &format!(
            r#"
            #define macro TEST() = takes(0) returns(0) {}
                {builtin}(MAIN)
            {}
            "#,
            "{", "}",
        );
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let mut lexer = Lexer::new(flattened_source);

        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // #define
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // macro
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // TEST
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // =
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // takes
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // 0
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // returns
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // 0
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // {
        let _ = lexer.next(); // whitespace

        // The builtin fn should be parsed as a `TokenKind::BuiltinFunction` here.
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let builtin_span = Span::new(74..74 + builtin.len(), None);
        assert_eq!(unwrapped, Token::new(TokenKind::BuiltinFunction(builtin.to_string()), builtin_span.clone()));

        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // MAIN
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // }
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // eof

        // We covered the whole source
        assert!(lexer.eof);
    }
}

#[test]
#[should_panic]
fn fails_to_parse_builtin_outside_macro_body() {
    let builtin_funcs = [
        "__codesize",
        "__tablesize",
        "__tablestart",
        "__FUNC_SIG",
        "__EVENT_HASH",
        "__ERROR",
        "__RIGHTPAD",
        "__CODECOPY_DYN_ARG",
        "__ASSERT_PC",
    ];

    for builtin in builtin_funcs {
        let source = &format!("{builtin}(MAIN)");
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let mut lexer = Lexer::new(flattened_source);

        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let fn_name_span = Span::new(0..builtin.len(), None);
        assert_eq!(unwrapped, Token::new(TokenKind::BuiltinFunction(builtin.to_string()), fn_name_span.clone()));

        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // MAIN
        let _ = lexer.next(); // close parenthesis

        // We covered the whole source
        assert!(lexer.eof);
    }
}

#[test]
#[should_panic]
fn fails_to_parse_invalid_builtin() {
    let builtin_funcs = ["__not_a_builtin", "__another_not_a_builtin", "__last_not_a_builtin"];

    for builtin in builtin_funcs {
        let source = &format!(
            r#"
            #define macro TEST() = takes(0) returns(0) {}
                {builtin}(MAIN)
            {}
            "#,
            "{", "}",
        );
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let mut lexer = Lexer::new(flattened_source);

        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // #define
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // macro
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // TEST
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // =
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // takes
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // 0
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // returns
        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // 0
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // {
        let _ = lexer.next(); // whitespace

        // The builtin fn should be parsed as a `TokenKind::BuiltinFunction` here.
        let tok = lexer.next();
        let unwrapped = tok.unwrap().unwrap();
        let builtin_span = Span::new(74..74 + builtin.len(), None);
        assert_eq!(unwrapped, Token::new(TokenKind::BuiltinFunction(builtin.to_string()), builtin_span.clone()));

        let _ = lexer.next(); // open parenthesis
        let _ = lexer.next(); // MAIN
        let _ = lexer.next(); // close parenthesis
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // }
        let _ = lexer.next(); // whitespace
        let _ = lexer.next(); // eof

        // We covered the whole source
        assert!(lexer.eof);
    }
}

#[test]
fn parses_builtin_function_in_macro_body_nested() {
    let source = r"
         #define macro TEST() = takes(0) returns(0) {
            __RIGHTPAD(__FUNC_SIG('hello()'))
         }";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens =
        lexer.into_iter().map(|x| x.unwrap()).filter(|x| matches!(x.kind, TokenKind::BuiltinFunction { .. })).collect::<Vec<Token>>();
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0].kind, TokenKind::BuiltinFunction("__RIGHTPAD".to_string()));
    assert_eq!(tokens[1].kind, TokenKind::BuiltinFunction("__FUNC_SIG".to_string()));
}

#[test]
fn parses_assert_pc_with_literal() {
    let source = r#"
        #define macro TEST() = takes(0) returns(0) {
            __ASSERT_PC(0x00)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // #define
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // macro
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // TEST
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // =
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // takes
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // returns
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace

    // Check __ASSERT_PC token
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::BuiltinFunction("__ASSERT_PC".to_string()));

    let _ = lexer.next(); // open parenthesis

    // Check literal argument
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::Literal(str_to_bytes32("00")));

    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // eof

    assert!(lexer.eof);
}

#[test]
fn parses_assert_pc_with_constant() {
    let source = r#"
        #define constant TARGET_PC = 0x10
        #define macro TEST() = takes(0) returns(0) {
            __ASSERT_PC([TARGET_PC])
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let mut lexer = Lexer::new(flattened_source);

    // Skip to the macro body
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // #define
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // constant
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // TARGET_PC
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // =
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // 0x10
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // #define
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // macro
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // TEST
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // =
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // takes
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // returns
    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // 0
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // {
    let _ = lexer.next(); // whitespace

    // Check __ASSERT_PC token
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::BuiltinFunction("__ASSERT_PC".to_string()));

    let _ = lexer.next(); // open parenthesis
    let _ = lexer.next(); // [

    // Check constant reference
    let tok = lexer.next();
    let unwrapped = tok.unwrap().unwrap();
    assert_eq!(unwrapped.kind, TokenKind::Ident("TARGET_PC".to_string()));

    let _ = lexer.next(); // ]
    let _ = lexer.next(); // close parenthesis
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // }
    let _ = lexer.next(); // whitespace
    let _ = lexer.next(); // eof

    assert!(lexer.eof);
}
