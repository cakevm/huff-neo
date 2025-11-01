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
fn test_arg_call_macro_invocation() {
    // Test simple argument macro invocation without arguments
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            MACRO1(NO_OP)
        }
        
        #define macro MACRO1(m) = takes(0) returns(0) {
            <m>()
        }
        
        #define macro NO_OP() = takes(0) returns(0) {
            0x42
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x42
    let expected_bytecode = "6042";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_arg_call_macro_invocation_with_args() {
    // Test argument macro invocation with arguments
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            WRAPPER(ADD_TWO)
        }
        
        #define macro WRAPPER(m) = takes(0) returns(0) {
            <m>(0x01, 0x02)
        }
        
        #define macro ADD_TWO(a, b) = takes(0) returns(0) {
            <a> <b> add
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x01 PUSH1 0x02 ADD
    let expected_bytecode = "6001600201";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_nested_arg_call_macro_invocation() {
    // Test nested argument macro invocations (GitHub issue #94 example)
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            MACRO1(NO_OP)
        }
        
        #define macro MACRO1(m) = takes(0) returns(0) {
            MACRO2(<m>())
        }
        
        #define macro MACRO2(m) = takes(0) returns(0) {
            <m>
        }
        
        #define macro NO_OP() = takes(0) returns(0) {
            0x01
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x01
    let expected_bytecode = "6001";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_arg_call_macro_invocation_passed_with_args() {
    // Test passing a macro invocation with arguments as an argument to another macro
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            OUTER(MULTIPLY)
        }
        
        #define macro OUTER(m) = takes(0) returns(0) {
            INNER(<m>(0x03, 0x04))
        }
        
        #define macro INNER(result) = takes(0) returns(0) {
            <result>
        }
        
        #define macro MULTIPLY(a, b) = takes(0) returns(0) {
            <a> <b> mul
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x03 PUSH1 0x04 MUL
    let expected_bytecode = "6003600402";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_arg_call_with_literal_and_arg_arguments() {
    // Test invoking a macro with mixed literal and argument arguments
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            WRAPPER(COMPLEX_OP, 0x05)
        }
        
        #define macro WRAPPER(m, val) = takes(0) returns(0) {
            <m>(0x10, <val>)
        }
        
        #define macro COMPLEX_OP(a, b) = takes(0) returns(0) {
            <a> <b> sub
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x10 PUSH1 0x05 SUB
    let expected_bytecode = "6010600503";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_multiple_arg_call_invocations() {
    // Test multiple macro invocations through arguments in the same macro
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            DISPATCHER(ADD_OP, SUB_OP)
        }
        
        #define macro DISPATCHER(m1, m2) = takes(0) returns(0) {
            <m1>(0x08, 0x03)
            <m2>(0x08, 0x03)
        }
        
        #define macro ADD_OP(a, b) = takes(0) returns(0) {
            <a> <b> add
        }
        
        #define macro SUB_OP(a, b) = takes(0) returns(0) {
            <a> <b> sub
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x08 PUSH1 0x03 ADD PUSH1 0x08 PUSH1 0x03 SUB
    let expected_bytecode = "60086003016008600303";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_arg_call_with_opcode_arguments() {
    // Test passing opcodes as arguments to a macro invoked through an argument
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            EXECUTOR(APPLY_OP)
        }
        
        #define macro EXECUTOR(m) = takes(0) returns(0) {
            <m>(caller, origin)
        }
        
        #define macro APPLY_OP(op1, op2) = takes(0) returns(0) {
            <op1> <op2> eq
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: CALLER ORIGIN EQ
    let expected_bytecode = "333214";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
}

#[test]
fn test_deeply_nested_arg_call_with_args() {
    // Test deeply nested macro invocations with arguments
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            LEVEL1(COMPUTE)
        }
        
        #define macro LEVEL1(m) = takes(0) returns(0) {
            LEVEL2(<m>(0x02))
        }
        
        #define macro LEVEL2(result) = takes(0) returns(0) {
            LEVEL3(<result>)
        }
        
        #define macro LEVEL3(final) = takes(0) returns(0) {
            <final> 0x03 add
        }
        
        #define macro COMPUTE(val) = takes(0) returns(0) {
            <val> dup1 mul
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: PUSH1 0x02 DUP1 MUL PUSH1 0x03 ADD
    let expected_bytecode = "60028002600301";
    assert_eq!(main_bytecode.to_lowercase(), expected_bytecode.to_lowercase());
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

    // COMPUTE_AND_STORE(ADD(0x05, SUB(0x05, ADD(ADD(0x0a, 0x16), 0x04))), 0x00)
    // Results in: 0x05, 0x05, 0x0a, 0x16, add, 0x04, add, sub, add, 0x00, mstore

    // COMPUTE_AND_STORE(
    //   ADD(
    //   - 6005 - PUSH1 0x05
    //     SUB(
    //       - 6005 - PUSH1 0x05
    //       ADD(
    //         ADD(
    //         - 600a - PUSH1 0x0a
    //         - 6016 - PUSH1 0x16
    //         - 01 - ADD
    //         )
    //       - 6004 - PUSH1 0x04
    //       - 01 - ADD
    //       )
    //       - 03 - SUB
    //     )
    //   - 01 - ADD
    //   )
    // - 5f - PUSH0 (0x00)
    // - 52 - MSTORE
    // )

    let expected_bytecode = "6005 6005 600a 6016 01 6004 01 03 01 5f 52".replace(" ", "");

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

#[test]
fn test_second_opcode_parameter() {
    let source: &str = r#"
        #define macro DO_NOTHING() = {
        }

        #define macro ADD_OP(a, b) = {
            <a>
            <b>
        }

        #define macro MAIN() = {
            ADD_OP(DO_NOTHING(), dup1)
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
    // 80 = DUP1
    assert_eq!(&r_bytes, "80");
}

#[test]
fn test_verbatim_before_macro_expansion() {
    let source = r#"
        #define constant CONST = 0x5678

        #define macro MAIN() = {
            MACRO(IDENTITY(CONST))
        }

        #define macro MACRO(arg) = {
            __VERBATIM(0x1234)
            <arg>
        }

        #define macro IDENTITY(arg) = {
            <arg>
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

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode:
    // __VERBATIM(0x1234) produces: 1234
    // IDENTITY(CONST) with CONST=0x5678 produces: 61 5678 (PUSH2 0x5678)
    assert_eq!(main_bytecode, "1234 61 5678".replace(" ", ""));
}

#[test]
fn test_nested_first_class_macro_invocation() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            OUTER(NO_OP)
        }

        #define macro OUTER(m) = takes(0) returns(0) {
            INNER(<m>)
        }

        #define macro INNER(m) = takes(0) returns(0) {
            <m>()
        }

        #define macro NO_OP() = takes(0) returns(0) {}
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main bytecode - should not hang or stack overflow
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // NO_OP macro is empty, so the bytecode should be empty
    assert_eq!(main_bytecode, "");
}

#[test]
fn test_nested_first_class_macro_with_bytecode() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            APPLY(UNARY, NO_OP)
        }

        #define macro APPLY(m, a) = takes(0) returns(0) {
            <m>(<a>())
            0x02
        }

        #define macro UNARY(a) = takes(0) returns(0) {
            <a>
            0x01
        }

        #define macro NO_OP() = takes(0) returns(0) {}
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let evm_version = EVMVersion::default();

    // Create main bytecode
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode:
    // APPLY expands to: UNARY(NO_OP()) + 0x02
    // UNARY(NO_OP()) expands to: NO_OP() + 0x01 = 0x01 (since NO_OP is empty)
    // So total: 0x01 + 0x02 = "6001" + "6002"
    assert_eq!(main_bytecode, "60016002");
}

#[test]
fn test_arg_not_passed_to_nested_macro() {
    // Verify that arguments do not leak into nested macros that don't receive them.
    // If M1 has parameter 'arg' and calls M2() without passing it, M2 should not have access to 'arg'.
    let source = r#"
        #define macro ARG() = takes(0) returns(0) {
            0xff
        }

        #define macro M1(arg) = takes(0) returns(0) {
            M2()
        }

        #define macro M2() = takes(0) returns(0) {
            <arg>
        }

        #define macro MAIN() = takes(0) returns(0) {
            M1(ARG())
        }
    "#;

    // Lex + Parse
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Codegen should fail because M2 tries to use <arg> which was not passed to it
    let codegen_result = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None);

    assert!(codegen_result.is_err());
    assert_eq!(codegen_result.unwrap_err().kind, CodegenErrorKind::MissingArgumentDefinition(String::from("arg")));
}

#[test]
fn test_arg_properly_passed_to_nested_macro() {
    // This test verifies that arguments CAN be passed through nested macros when done correctly
    let source = r#"
        #define macro ARG() = takes(0) returns(0) {
            0xff
        }

        #define macro M1(arg) = takes(0) returns(0) {
            M2(<arg>)
        }

        #define macro M2(x) = takes(0) returns(0) {
            <x>
        }

        #define macro MAIN() = takes(0) returns(0) {
            M1(ARG())
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

    // This should succeed because M1 properly passes <arg> to M2
    let main_bytecode = Codegen::generate_main_bytecode(&evm_version, &contract, None).unwrap();

    // Expected bytecode: ARG() expands to 0xff, which is PUSH1 0xff = 60ff
    assert_eq!(main_bytecode, "60ff");
}
