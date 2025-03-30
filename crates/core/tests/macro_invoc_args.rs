use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::Parser;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;
use std::str::FromStr;

#[test]
fn test_undefined_macro_arg() {
    let source = r#"
        #define macro MACRO1(arg) = takes(0) returns(0) {
            <undefined>
        }

        #define macro MAIN() = takes(0) returns(0) {
            MACRO1(0x01)
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::MissingArgumentDefinition(String::from("undefined")));
}

#[test]
fn test_opcode_macro_args() {
    let source = r#"
        #define macro RETURN1(zero) = takes(0) returns(0) {
            <zero> mstore
            0x20 <zero> return
        }

        #define macro MAIN() = takes(0) returns(0) {
            RETURN1(returndatasize)
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60088060093d393df360ff3d5260203df3";

    // Create bytecode
    let bytecode = format!("60088060093d393df360ff{main_bytecode}");

    // Check the bytecode
    assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_all_opcodes_in_macro_args() {
    for o in OPCODES {
        let source = format!(
            r#"
            #define macro RETURN1(zero) = takes(0) returns(0) {{
                <zero>
            }}

            #define macro MAIN() = takes(0) returns(0) {{
                RETURN1({o})
            }}
        "#
        );

        // Lex + Parse
        let flattened_source = FullFileSource { source: &source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);
        let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
        let mut parser = Parser::new(tokens, None);
        let mut contract = parser.parse().unwrap();
        contract.derive_storage_pointers();

        let evm_version = EVMVersion::default();

        // Create main and constructor bytecode
        let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

        // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
        let expected_bytecode = format!("60088060093d393df360ff{}", Opcode::from_str(o).unwrap());

        // Create bytecode
        let bytecode = format!("60088060093d393df360ff{main_bytecode}");

        // Check the bytecode
        assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
    }
}

#[test]
fn test_constant_macro_arg() {
    let source = r#"
            #define constant A = 0x02

            #define macro RETURN1(zero) = takes(0) returns(0) {
                <zero>
            }

            #define macro MAIN() = takes(0) returns(0) {
                RETURN1(A)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60088060093d393df360ff6002";

    // Create bytecode
    let bytecode = format!("60088060093d393df360ff{main_bytecode}");

    // Check the bytecode
    assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_bubbled_label_call_macro_arg() {
    let source = r#"
            #define macro MACRO_A(zero) = takes(0) returns(0) {
                MACRO_B(<zero>)
            }

            #define macro MACRO_B(zero) = takes(0) returns(0) {
                <zero>
            }

            #define macro MAIN() = takes(0) returns(0) {
                label:
                    MACRO_A(label)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60088060093d393df360ff5b610000";

    // Create bytecode
    let bytecode = format!("60088060093d393df360ff{main_bytecode}");

    // Check the bytecode
    assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_bubbled_literal_macro_arg() {
    let source = r#"
            #define macro MACRO_A(zero) = takes(0) returns(0) {
                MACRO_B(<zero>)
            }

            #define macro MACRO_B(zero) = takes(0) returns(0) {
                <zero>
            }

            #define macro MAIN() = takes(0) returns(0) {
                MACRO_A(0x420)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60088060093d393df360ff610420";

    // Create bytecode
    let bytecode = format!("60088060093d393df360ff{main_bytecode}");

    // Check the bytecode
    assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_bubbled_opcode_macro_arg() {
    let source = r#"
            #define macro MACRO_A(zero) = takes(0) returns(0) {
                MACRO_B(<zero>)
            }

            #define macro MACRO_B(zero) = takes(0) returns(0) {
                <zero>
            }

            #define macro MAIN() = takes(0) returns(0) {
                MACRO_A(returndatasize)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60088060093d393df360ff3d";

    // Create bytecode
    let bytecode = format!("60088060093d393df360ff{main_bytecode}");

    // Check the bytecode
    assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_bubbled_constant_macro_arg() {
    let source = r#"
            #define constant A = 0x02

            #define macro MACRO_A(zero) = takes(0) returns(0) {
                MACRO_B(<zero>)
            }

            #define macro MACRO_B(zero) = takes(0) returns(0) {
                <zero>
            }

            #define macro MAIN() = takes(0) returns(0) {
                MACRO_A(A)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60088060093d393df360ff6002";

    // Create bytecode
    let bytecode = format!("60088060093d393df360ff{main_bytecode}");

    // Check the bytecode
    assert_eq!(bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_bubbled_arg_with_different_name() {
    let source = r#"
          #define macro MACRO_A(arg_a) = takes(0) returns(0) {
            <arg_a>
          }
          #define macro MACRO_B(arg_b) =takes(0) returns(0) {
            MACRO_A(<arg_b>)
          }
          #define macro MAIN() = takes(0) returns(0){
            MACRO_B(0x01)
          }
      "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "6001";

    // Check the bytecode
    assert_eq!(main_bytecode, expected_bytecode);
}

#[test]
fn test_nested_macro_call_no_args() {
    let source = r#"
          #define macro COMPUTE_FIXED(a) = takes(0) returns(0) {
            0x01
            0x01
            <a>
          }

          #define macro ADD() = takes(0) returns(0) {
            add
          }

          #define macro SUB() = takes(0) returns(0) {
            sub
          }

          #define macro MAIN() = takes(0) returns(0) {
            COMPUTE_FIXED(ADD())
            COMPUTE_FIXED(SUB())
          }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60016001016001600103";

    // Check the bytecode
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_nested_macro_calls() {
    let source = r#"
            #define macro ADD(a, b) = takes(0) returns(1) {
                <a> <b> add
            }

            #define macro SUB(a, b) = takes(0) returns(1) {
                <a> <b> sub
            }

            #define macro COMPUTE_AND_STORE(operation, offset) = takes(0) returns(0) {
                <operation>
                <offset> mstore
            }

            #define macro MAIN() = takes(0) returns(0) {
                COMPUTE_AND_STORE(ADD(0x05, 0x06), 0x00)
                COMPUTE_AND_STORE(SUB(0x05, 0x06), 0x20)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60056006015f526005600603602052";

    // Check the bytecode
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_very_nested_macro_calls() {
    let source = r#"
            #define macro ADD(a, b) = takes(0) returns(1) {
                <a> <b> add
            }

            #define macro SUB(a, b) = takes(0) returns(1) {
                <a> <b> sub
            }

            #define macro COMPUTE_AND_STORE(operation, offset) = takes(0) returns(0) {
                <operation> <offset> mstore
            }

            #define macro MAIN() = takes(0) returns(0) {
                COMPUTE_AND_STORE(ADD(0x05, SUB(0x05, ADD(ADD(0x0a, 0x16), 0x04))), 0x00)
            }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main and constructor bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Full expected bytecode output (generated from huff-neo) (placed here as a reference)
    let expected_bytecode = "60056005600a60160160040103015f52";

    // Check the bytecode
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_nested_macro_call_missing_macro_definition() {
    let source = r#"
          #define macro MACRO1(a) = takes(0) returns(0) {
            <a>
          }

          #define macro MAIN() = takes(0) returns(0) {
            MACRO1(MISSING_MACRO())
          }
        "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::MissingMacroDefinition(String::from("MISSING_MACRO")));
}

#[test]
fn test_push0_arg() {
    let source: &str = r#"
        #define macro PUSH0(a) = {
            <a>
        }

        #define macro MAIN() = {
            0x00
            PUSH0(0x00)
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();

    // Derive storage pointers
    contract.derive_storage_pointers();

    // Instantiate Codegen
    let cg = Codegen::new();

    // The codegen instance should have no artifact
    assert!(cg.artifact.is_none());

    // Have Codegen create the runtime bytecode
    let r_bytes = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None).unwrap();
    // 2x 5f = PUSH0
    assert_eq!(&r_bytes, "5f5f");
}

#[test]
fn test_unexpected_argument_none_expected() {
    let source: &str = r#"
        #define macro TEST() = {
        }

        #define macro MAIN() = {
            TEST(0x01)
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();

    // Derive storage pointers
    contract.derive_storage_pointers();

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::InvalidArgumentCount(String::from("TEST"), 0, 1));
}

#[test]
fn test_unexpected_arguments() {
    let source: &str = r#"
        #define macro TEST(arg1) = {
            <arg1>
        }

        #define macro MAIN() = {
            TEST(0x01, 0x02)
        }
    "#;

    // Parse tokens
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);

    // Parse the AST
    let mut contract = parser.parse().unwrap();

    // Derive storage pointers
    contract.derive_storage_pointers();

    // Codegen should fail with an error
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::InvalidArgumentCount(String::from("TEST"), 1, 2));
}
