// Regression test for https://github.com/cakevm/huff-neo/issues/145
use huff_neo_codegen::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_keccak256_and_sha3_produce_same_bytecode() {
    // Test that both keccak256 and sha3 produce the same bytecode (0x20)
    let source_with_keccak256: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x00 0x00 keccak256
            pop
        }
    "#;

    let source_with_sha3: &str = r#"
        #define macro MAIN() = takes(0) returns(0) {
            0x00 0x00 sha3
            pop
        }
    "#;

    // Compile with keccak256
    let flattened_source = FullFileSource { source: source_with_keccak256, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let keccak256_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Compile with sha3
    let flattened_source = FullFileSource { source: source_with_sha3, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let sha3_bytecode = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // Both should produce identical bytecode
    assert_eq!(keccak256_bytecode, sha3_bytecode);

    // Verify the bytecode is correct: 5f (PUSH0) 5f (PUSH0) 20 (KECCAK256/SHA3) 50 (POP)
    assert_eq!(keccak256_bytecode, "5f5f2050");
    assert_eq!(sha3_bytecode, "5f5f2050");
}
