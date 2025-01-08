use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::{abi::*, prelude::*};

#[test]
fn build_abi_from_ast() {
    let source = "#define function test(uint256[2][],string) view returns(uint256)";

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source.source, None);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let contract = parser.parse().unwrap();
    let abi = Abi::from(contract);

    assert_eq!(
        abi.functions.get("test").unwrap().inputs[0].kind,
        FunctionParamType::Array(Box::new(FunctionParamType::Uint(256)), vec![2, 0])
    );
    assert_eq!(abi.functions.get("test").unwrap().inputs[1].kind, FunctionParamType::String);
}
