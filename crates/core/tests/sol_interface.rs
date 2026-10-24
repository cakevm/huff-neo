use std::sync::Arc;

use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_utils::abi::Abi;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_interface_declares_tuples_as_structs() {
    let source: &str = r#"
        #define function constructor(uint256) nonpayable returns ()
        #define function swap((address,uint256) order, bytes data) nonpayable returns ((uint256,(address,uint256))[])
        #define event Filled((address,uint256) order)
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let tokens = Lexer::new(flattened_source).map(|x| x.unwrap()).collect::<Vec<Token>>();
    let contract = Parser::new(tokens, None).parse().unwrap();
    let file = FileSource { path: "./src/Swap.huff".to_string(), ..Default::default() };
    let artifact = Artifact { abi: Some(Abi::from(contract)), file: Arc::new(file), ..Default::default() };

    let interfaces = gen_sol_interfaces(&vec![Arc::new(artifact)], None);

    // Solidity has no anonymous tuples, and the constructor is not part of an interface
    assert_eq!(
        interfaces[0].2,
        [
            "interface ISwap {",
            "\tstruct Tuple0 {\n\t\taddress _0;\n\t\tuint256 _1;\n\t}",
            "\tstruct Tuple1 {\n\t\tuint256 _0;\n\t\tTuple0 _1;\n\t}",
            "\tevent Filled(Tuple0);",
            "\tfunction swap(Tuple0 memory, bytes memory) external returns (Tuple1[] memory);",
            "}",
        ]
        .join("\n")
    );
}
