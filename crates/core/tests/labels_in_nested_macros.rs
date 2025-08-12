use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::error::CodegenErrorKind;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_simple_label_cross_macro_reference() {
    // Label referenced across macro boundaries
    let source = r#"
        #define macro JUMP_TO_LABEL() = takes(0) returns(0) {
            target_label jump
        }

        #define macro MAIN() = takes(0) returns(0) {
            JUMP_TO_LABEL()
            
            target_label:
                0x42
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected bytecode: PUSH2 0x0004, JUMP, JUMPDEST, PUSH1 0x42
    assert_eq!(bytecode, "610004565b6042");
}

#[test]
fn test_nested_label() {
    // Macro with label jump passed as argument
    let source = r#"
        #define macro ERROR() = {
            label_error jump
        }

        #define macro RUN(act) = {
            <act>
        }

        #define macro MAIN() = {
            RUN(ERROR())

            stop

            label_error:
                0x00 0x00 revert
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(result.is_ok());
    let bytecode = result.unwrap();

    // Expected: PUSH2 0x0005, JUMP, JUMPDEST, PUSH0, PUSH0, REVERT
    // 61 0005 56 5b 5f 5f fd
    assert_eq!(bytecode, "61000556005b5f5ffd", "Bytecode should correctly resolve label jumps");
}

#[test]
fn test_label_in_nested_macro_with_args() {
    // Test labels in nested macros with arguments
    let source = r#"
        #define macro HELPER(val) = takes(0) returns(0) {
            loop:
                <val>
                loop jump
        }

        #define macro RUN(input) = takes(0) returns(0) {
            HELPER(<input>)
        }

        #define macro MAIN() = takes(0) returns(0) {
            RUN(0x42)
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(result.is_ok());

    let bytecode = result.unwrap();

    // Expected: JUMPDEST, PUSH1 0x42, PUSH 0x0000, JUMP
    assert_eq!(bytecode, "5b604261000056");
}

#[test]
fn test_multiple_labels_in_nested_macro() {
    // Test multiple labels in nested macro calls
    let source = r#"
        #define macro COMPLEX() = takes(0) returns(0) {
            start:
                0x01
                dup1
                0x02 eq
                end jumpi
                
            middle:
                0x03 add
                start jump
                
            end:
                pop
        }

        #define macro MAIN() = takes(0) returns(0) {
            COMPLEX()
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0x01, DUP1, PUSH1 0x02, EQ, JUMPI, JUMPDEST, PUSH1 0x03, ADD, PUSH2 0x0000, JUMP, JUMPDEST, POP
    assert_eq!(bytecode, "5b600180600214610013575b600301610000565b50");
}

#[test]
fn test_label_scope_isolation() {
    // Test that labels in different macro scopes don't interfere
    let source = r#"
        #define macro FIRST() = takes(0) returns(0) {
            loop:
                0x01
                loop jump
        }

        #define macro SECOND() = takes(0) returns(0) {
            loop:  // Same label name but different scope
                0x02
                loop jump
        }

        #define macro MAIN() = takes(0) returns(0) {
            FIRST()
            SECOND()
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // This should work - labels should be properly scoped
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0x01, PUSH2 0x0000, JUMP, JUMPDEST, PUSH1 0x02, PUSH2 0x007, JUMP
    assert_eq!(bytecode, "5b6001610000565b600261000756");
}

#[test]
fn test_duplicate_label_in_same_macro_parser_error() {
    // Duplicate labels in the same macro should be caught by the parser
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            my_label:       
            0x01
            my_label:       // ERROR: Duplicate label
            0x02
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // The parser should catch duplicate labels at parse time
    let parse_result = parser.parse();
    assert!(parse_result.is_err());
    assert_eq!(parse_result.unwrap_err().kind, ParserErrorKind::DuplicateLabel("my_label".to_string()));
}

#[test]
fn test_multiple_macro_invocations_with_same_label() {
    // Multiple invocations of the same macro that defines a label should work
    // Each invocation gets its own scope
    let source = r#"
        #define macro DEF_AND_JUMP() = takes(0) returns(0) {
            my_label:
            0x42            
            my_label        // Jump to my_label - should jump to THIS instance's label
            jump
        }

        #define macro MAIN() = takes(0) returns(0) {
            DEF_AND_JUMP()  // First invocation
            DEF_AND_JUMP()  // Second invocation
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0x42, PUSH2 0x0000, JUMP, JUMPDEST, PUSH1 0x42, PUSH2 0x0007, JUMP
    assert_eq!(bytecode, "5b6042610000565b604261000756");
}

#[test]
fn test_label_shadowing_across_scopes() {
    // Labels in different scopes should be allowed (shadowing)
    let source = r#"
        #define macro INNER() = takes(0) returns(0) {
            shared_label:
            0xAA
            shared_label    // Should jump to INNER's shared_label
            jump
        }

        #define macro MAIN() = takes(0) returns(0) {
            shared_label:
            0xBB
            INNER()         
            shared_label    // Should jump to MAIN's shared_label
            jump
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0xBB, JUMPDEST, PUSH1 0xAA, PUSH2 0x0003, JUMP, PUSH2 0x0000, JUMP
    assert_eq!(bytecode, "5b60bb5b60aa6100035661000056");
}

#[test]
fn test_nested_label_shadowing_three_levels() {
    // Test deeper nesting with label shadowing
    let source = r#"
        #define macro LEVEL3() = takes(0) returns(0) {
            label_a:
            0x03
        }
        
        #define macro LEVEL2() = takes(0) returns(0) {
            label_a:     
            0x02
            LEVEL3()
            label_a      // Should jump to LEVEL2's label_a
            jump
        }
        
        #define macro MAIN() = takes(0) returns(0) {
            label_a:     
            0x01
            LEVEL2()
            label_a      // Should jump to MAIN's label_a  
            jump
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0x01, JUMPDEST, PUSH1 0x02, JUMPDEST, PUSH1 0x03, PUSH2 0x0003, JUMP, PUSH2 0x0000, JUMP
    assert_eq!(bytecode, "5b60015b60025b60036100035661000056");
}

#[test]
fn test_sibling_macro_label_visibility() {
    // Test that sibling macros can see each other's labels when invoked from same parent
    let source = r#"
        #define macro DEFINE_LABEL() = takes(0) returns(0) {
            shared_label:
            0x42
        }

        #define macro JUMP_TO_LABEL() = takes(0) returns(0) {
            shared_label jump
        }

        #define macro MAIN() = takes(0) returns(0) {
            DEFINE_LABEL()    // First macro defines the label
            JUMP_TO_LABEL()   // Second macro should be able to jump to it
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0x42, PUSH2 0x0000, JUMP
    assert_eq!(bytecode, "5b604261000056");
}

#[test]
fn test_multiple_macro_invocations_with_cross_references() {
    // Test valid cross-references between sibling macro invocations
    let source = r#"
        #define macro DEF_LBL() = takes(0) returns(0) {
            lbl:
            0x42
        }

        #define macro JUMP_TO_LBL() = takes(0) returns(0) {
            lbl jump
        }

        #define macro MAIN() = takes(0) returns(0) {
            DEF_LBL()        // Defines lbl
            JUMP_TO_LBL()    // Can jump to lbl from sibling
            JUMP_TO_LBL()    // Can also jump to lbl from another sibling
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST, PUSH1 0x42, PUSH2 0x0000, JUMP, PUSH2 0x0000, JUMP
    assert_eq!(bytecode, "5b60426100005661000056");
}

#[test]
fn test_duplicate_labels_in_sibling_scopes_error() {
    // Test that duplicate labels in sibling scopes cause an error when cross-referenced
    let source = r#"
        #define macro DEF_LBL() = takes(0) returns(0) {
            lbl:
            0x42
        }

        #define macro JUMP_TO_LBL() = takes(0) returns(0) {
            lbl jump    // Trying to reference lbl - ambiguous if multiple siblings define it
        }

        #define macro MAIN() = takes(0) returns(0) {
            DEF_LBL()       // First invocation defines lbl
            DEF_LBL()       // Second invocation also defines lbl
            JUMP_TO_LBL()   // This tries to reference lbl - should error due to ambiguity
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    // Should fail with duplicate label error when there's cross-referencing
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, CodegenErrorKind::DuplicateLabelAcrossSiblings("lbl".to_string()));
}

#[test]
fn test_parent_uses_deeply_nested_label() {
    // Test that a parent macro can use a label defined in a deeply nested macro
    let source = r#"
        #define macro INNER() = takes(0) returns(0) {
            inner_label:
            0x42
        }

        #define macro OUTER() = takes(0) returns(0) {
            INNER()
        }

        #define macro MAIN() = takes(0) returns(0) {
            OUTER()
            inner_label jump  // Jump to label defined in nested INNER macro
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);
    assert!(result.is_ok());

    let bytecode = result.unwrap();
    // Expected: JUMPDEST (inner_label at offset 0), PUSH1 0x42, PUSH2 0x0000, JUMP
    // The jump should target offset 0 where inner_label is defined in the nested INNER macro
    assert_eq!(bytecode, "5b604261000056");
}
