use huff_neo_lexer::Lexer;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_rejects_builtin_function_as_macro_name() {
    // Test that __FUNC_SIG (and other builtins) cannot be used as macro names
    let source = "#define macro __FUNC_SIG() = takes(0) returns(0) {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, ParserErrorKind::InvalidMacroName));
    assert!(err.hint.is_some());
    assert!(err.hint.unwrap().contains("reserved"));
}

#[test]
fn test_rejects_multiple_builtin_names_as_macros() {
    // Test various builtin function names
    let builtin_names = vec![
        "__FUNC_SIG",
        "__EVENT_HASH",
        "__ERROR",
        "__tablesize",
        "__codesize",
        "__tablestart",
        "__RIGHTPAD",
        "__LEFTPAD",
        "__CODECOPY_DYN_ARG",
        "__VERBATIM",
        "__BYTES",
        "__ASSERT_PC",
        "__NOOP",
    ];

    for name in builtin_names {
        let source = format!("#define macro {}() = takes(0) returns(0) {{}}", name);
        let flattened_source = FullFileSource { source: &source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);
        let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
        let mut parser = Parser::new(tokens, None);
        let result = parser.parse();

        assert!(result.is_err(), "Expected error for builtin name: {}, but parsing succeeded", name);
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ParserErrorKind::InvalidMacroName), "Expected InvalidMacroName error for {}, got {:?}", name, err.kind);
    }
}

#[test]
fn test_allows_non_builtin_macro_names() {
    // Test that normal macro names still work
    let source = "#define macro MY_MACRO() = takes(0) returns(0) {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_ok(), "Valid macro name should be accepted");
}

#[test]
fn test_rejects_builtin_as_function_macro_name() {
    // Test that builtin names are also rejected for function macros (fn keyword)
    let source = "#define fn __FUNC_SIG() = takes(0) returns(0) {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, ParserErrorKind::InvalidMacroName));
}

#[test]
fn test_rejects_builtin_as_test_macro_name() {
    // Test that builtin names are also rejected for test macros
    let source = "#define test __EVENT_HASH() = {}";
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let result = parser.parse();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.kind, ParserErrorKind::InvalidMacroName));
}
