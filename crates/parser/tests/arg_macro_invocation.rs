use huff_neo_lexer::Lexer;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn parse_arg_call_invocation_no_args() {
    let source = r#"
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>()
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("WRAPPER").unwrap();

    assert_eq!(macro_def.name, "WRAPPER");
    assert_eq!(macro_def.parameters.len(), 1);
    assert_eq!(macro_def.statements.len(), 1);

    // Check that <m>() is parsed as ArgMacroInvocation
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 0);
    } else {
        panic!("Expected ArgMacroInvocation for <m>()");
    }
}

#[test]
fn parse_arg_call_invocation_with_literal_args() {
    let source = r#"
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>(0x01, 0x02)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("WRAPPER").unwrap();

    assert_eq!(macro_def.statements.len(), 1);

    // Check that <m>(0x01, 0x02) is parsed correctly
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 2);

        // Check first argument is literal 0x01
        if let MacroArg::Literal(bytes) = &args[0] {
            assert_eq!(bytes[31], 0x01);
        } else {
            panic!("Expected literal 0x01");
        }

        // Check second argument is literal 0x02
        if let MacroArg::Literal(bytes) = &args[1] {
            assert_eq!(bytes[31], 0x02);
        } else {
            panic!("Expected literal 0x02");
        }
    } else {
        panic!("Expected ArgMacroInvocation");
    }
}

#[test]
fn parse_arg_call_invocation_with_opcode_args() {
    let source = r#"
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>(caller, origin)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("WRAPPER").unwrap();

    assert_eq!(macro_def.statements.len(), 1);

    // Check that <m>(caller, origin) is parsed correctly
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 2);

        // Check arguments - in this context they're parsed as Ident
        if let MacroArg::Ident(ident) = &args[0] {
            assert_eq!(ident, "caller");
        } else {
            panic!("Expected Ident 'caller'");
        }

        if let MacroArg::Ident(ident) = &args[1] {
            assert_eq!(ident, "origin");
        } else {
            panic!("Expected Ident 'origin'");
        }
    } else {
        panic!("Expected ArgMacroInvocation");
    }
}

#[test]
fn parse_arg_call_invocation_with_arg_ref() {
    let source = r#"
        #define macro WRAPPER(m, val) = takes(0) returns(0) {
            <m>(0x10, <val>)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("WRAPPER").unwrap();

    assert_eq!(macro_def.parameters.len(), 2);
    assert_eq!(macro_def.statements.len(), 1);

    // Check that <m>(0x10, <val>) is parsed correctly
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 2);

        // Check first argument is literal
        if let MacroArg::Literal(bytes) = &args[0] {
            assert_eq!(bytes[31], 0x10);
        } else {
            panic!("Expected literal 0x10");
        }

        // Check second argument is ArgCall
        if let MacroArg::ArgCall(arg_call) = &args[1] {
            assert_eq!(arg_call.name, "val");
        } else {
            panic!("Expected ArgCall for val");
        }
    } else {
        panic!("Expected ArgMacroInvocation");
    }
}

#[test]
fn parse_multiple_arg_call_invocations() {
    let source = r#"
        #define macro DISPATCHER(m1, m2) = takes(0) returns(0) {
            <m1>(0x01)
            <m2>(0x02)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("DISPATCHER").unwrap();

    assert_eq!(macro_def.parameters.len(), 2);
    assert_eq!(macro_def.statements.len(), 2);

    // Check first statement: <m1>(0x01)
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "DISPATCHER");
        assert_eq!(arg_name, "m1");
        assert_eq!(args.len(), 1);

        if let MacroArg::Literal(bytes) = &args[0] {
            assert_eq!(bytes[31], 0x01);
        } else {
            panic!("Expected literal 0x01");
        }
    } else {
        panic!("Expected ArgMacroInvocation for first statement");
    }

    // Check second statement: <m2>(0x02)
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[1].ty {
        assert_eq!(parent_macro, "DISPATCHER");
        assert_eq!(arg_name, "m2");
        assert_eq!(args.len(), 1);

        if let MacroArg::Literal(bytes) = &args[0] {
            assert_eq!(bytes[31], 0x02);
        } else {
            panic!("Expected literal 0x02");
        }
    } else {
        panic!("Expected ArgMacroInvocation for second statement");
    }
}

#[test]
fn parse_nested_macro_with_arg_invocation() {
    let source = r#"
        #define macro OUTER(m) = takes(0) returns(0) {
            INNER(<m>(0x05))
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("OUTER").unwrap();

    assert_eq!(macro_def.statements.len(), 1);

    // Check that INNER(<m>(0x05)) is parsed correctly
    if let StatementType::MacroInvocation(mi) = &macro_def.statements[0].ty {
        assert_eq!(mi.macro_name, "INNER");
        assert_eq!(mi.args.len(), 1);

        // Check the argument is ArgCallMacroInvocation
        if let MacroArg::ArgCallMacroInvocation(arg_name, args) = &mi.args[0] {
            assert_eq!(arg_name, "m");
            assert_eq!(args.len(), 1);

            if let MacroArg::Literal(bytes) = &args[0] {
                assert_eq!(bytes[31], 0x05);
            } else {
                panic!("Expected literal 0x05");
            }
        } else {
            panic!("Expected ArgCallMacroInvocation as argument to INNER");
        }
    } else {
        panic!("Expected MacroInvocation for INNER");
    }
}

#[test]
fn parse_arg_call_with_macro_call_arg() {
    let source = r#"
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>(HELPER())
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("WRAPPER").unwrap();

    assert_eq!(macro_def.statements.len(), 1);

    // Check that <m>(HELPER()) is parsed correctly
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 1);

        // Check the argument is a MacroCall
        if let MacroArg::MacroCall(macro_call) = &args[0] {
            assert_eq!(macro_call.macro_name, "HELPER");
            assert_eq!(macro_call.args.len(), 0);
        } else {
            panic!("Expected MacroCall for HELPER");
        }
    } else {
        panic!("Expected ArgMacroInvocation");
    }
}

#[test]
fn parse_arg_call_with_ident_arg() {
    let source = r#"
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>(SOME_CONST)
        }
    "#;
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    let contract = parser.parse().unwrap();
    let macro_def = contract.macros.get("WRAPPER").unwrap();

    assert_eq!(macro_def.statements.len(), 1);

    // Check that <m>(SOME_CONST) is parsed correctly
    if let StatementType::ArgMacroInvocation(parent_macro, arg_name, args) = &macro_def.statements[0].ty {
        assert_eq!(parent_macro, "WRAPPER");
        assert_eq!(arg_name, "m");
        assert_eq!(args.len(), 1);

        // Check the argument is an Ident
        if let MacroArg::Ident(ident) = &args[0] {
            assert_eq!(ident, "SOME_CONST");
        } else {
            panic!("Expected Ident for SOME_CONST");
        }
    } else {
        panic!("Expected ArgMacroInvocation");
    }
}
