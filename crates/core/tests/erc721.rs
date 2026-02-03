use huff_neo_codegen::Codegen;
use huff_neo_core::*;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::file::file_provider::FileSystemFileProvider;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::file::remapper;
use huff_neo_utils::prelude::*;
use std::collections::HashSet;
use std::{path::PathBuf, sync::Arc};

#[test]
fn test_erc721_compile() {
    let file_provider = Arc::new(FileSystemFileProvider {});
    let file_sources: Vec<Arc<FileSource>> =
        Compiler::fetch_sources(vec![PathBuf::from("../../resources/erc721/ERC721.huff".to_string())], file_provider.clone())
            .iter()
            .map(|p| p.clone().unwrap())
            .collect();

    // Recurse file deps + generate flattened source
    let file_source = file_sources.first().unwrap();
    let recursed_file_source =
        Compiler::recurse_deps(Arc::clone(file_source), &remapper::Remapper::new("./"), file_provider, HashSet::new()).unwrap();
    let flattened = FileSource::fully_flatten(Arc::clone(&recursed_file_source));
    let full_source = FullFileSource { source: &flattened.0, file: Some(Arc::clone(file_source)), spans: flattened.1 };
    let lexer = Lexer::new(full_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, Some("../../resources/erc20".to_string()));
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    // Create main and constructor bytecode using the paris compatible evm version
    let paris_evm = EVMVersion::new(SupportedEVMVersions::Paris);

    let paris_main_bytecode = Codegen::generate_main_bytecode(&paris_evm, &contract, None, false).unwrap();
    let (paris_constructor_bytecode, paris_has_custom_bootstrap) =
        Codegen::generate_constructor_bytecode(&paris_evm, &contract, None, false).unwrap();

    // Create main and constructor bytecode using the shanghai compatible evm version
    let shanghai_evm = EVMVersion::new(SupportedEVMVersions::Shanghai);

    let shanghai_main_bytecode = Codegen::generate_main_bytecode(&shanghai_evm, &contract, None, false).unwrap();
    let (shanghai_constructor_bytecode, has_custom_bootstrap) =
        Codegen::generate_constructor_bytecode(&shanghai_evm, &contract, None, false).unwrap();

    // Churn
    let mut cg = Codegen::new();
    let paris_artifact = cg
        .churn(
            Arc::clone(file_source),
            vec![],
            &paris_main_bytecode,
            &paris_constructor_bytecode,
            paris_has_custom_bootstrap,
            None,
            None,
            false,
        )
        .unwrap();

    // Full expected bytecode output (generated from huff-neo)
    let expected_paris_bytecode = "336000556103b180600e3d393df360003560e01c8063a9059cbb146100a057806342842e0e146101a3578063b88d4fde146101a9578063095ea7b31461027b578063a22cb46514610310578063081812fc146102f357806340c10f19146101af57806370a082311461025e5780636352211e1461039457806306fdde031461035e57806395d89b4114610364578063c87b56dd1461036a57806301ffc9a714610370578063e985e9c514610376575b6044356024356004358083600160005260006020015260406000205491146100c75761019d565b8033146101005733816000526000602001526040600020546101005782600260005260006020015260406000205433146101005761019d565b6001816003600052600060200152604060002054038160036000526000602001526040600020558160036000526000602001526040600020546001018260036000526000602001526040600020558183600160005260006020015260406000205560008360026000526000602001526040600020557fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60206000a4005b60006000fd5b60006000fd5b60006000fd5b60005433146101be5760006000fd5b6024356004356000826001600052600060200152604060002054156101e257610258565b8160036000526000602001526040600020546001018260036000526000602001526040600020558183600160005260006020015260406000205560008360026000526000602001526040600020557fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60006000a4005b60006000fd5b600435600360005260006020015260406000205460005260206000f35b6024358060016000526000602001526040600020548033143382600052600060200152604060002054176102ae576102ed565b60043580836002600052600060200152604060002055907f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b92560006000a4005b60006000fd5b600435600260005260006020015260406000205460005260206000f35b60243560043533600052600060200152604060002055600435336024356000527f17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c3160006000a4005b60006000fd5b60006000fd5b60006000fd5b60006000fd5b60006000fd5b60243560043560005260006020015260406000205460005260206000f35b600435600160005260006020015260406000205460005260206000f3";

    assert_eq!(paris_artifact.bytecode.to_lowercase(), expected_paris_bytecode.to_lowercase());

    let shanghai_artifact = cg
        .churn(
            Arc::clone(file_source),
            vec![],
            &shanghai_main_bytecode,
            &shanghai_constructor_bytecode,
            has_custom_bootstrap,
            None,
            None,
            false,
        )
        .unwrap();

    let expected_shanghai_bytecode = "335f5561034480600d3d393df35f3560e01c8063a9059cbb1461009f57806342842e0e14610183578063b88d4fde14610187578063095ea7b31461023a578063a22cb465146102bd578063081812fc146102a557806340c10f191461018b57806370a08231146102225780636352211e1461032c57806306fdde031461030357806395d89b4114610307578063c87b56dd1461030b57806301ffc9a71461030f578063e985e9c514610313575b604435602435600435808360015f525f6020015260405f205491146100c35761017f565b8033146100f65733815f525f6020015260405f20546100f6578260025f525f6020015260405f205433146100f65761017f565b60018160035f525f6020015260405f2054038160035f525f6020015260405f20558160035f525f6020015260405f20546001018260035f525f6020015260405f2055818360015f525f6020015260405f20555f8360025f525f6020015260405f20557fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60205fa4005b5f5ffd5b5f5ffd5b5f5ffd5b5f543314610197575f5ffd5b6024356004355f8260015f525f6020015260405f2054156101b75761021e565b8160035f525f6020015260405f20546001018260035f525f6020015260405f2055818360015f525f6020015260405f20555f8360025f525f6020015260405f20557fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef5f5fa4005b5f5ffd5b60043560035f525f6020015260405f20545f5260205ff35b6024358060015f525f6020015260405f205480331433825f525f6020015260405f205417610267576102a1565b600435808360025f525f6020015260405f2055907f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b9255f5fa4005b5f5ffd5b60043560025f525f6020015260405f20545f5260205ff35b602435600435335f525f6020015260405f2055600435336024355f527f17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c315f5fa4005b5f5ffd5b5f5ffd5b5f5ffd5b5f5ffd5b5f5ffd5b6024356004355f525f6020015260405f20545f5260205ff35b60043560015f525f6020015260405f20545f5260205ff3";

    assert_eq!(shanghai_artifact.bytecode.to_lowercase(), expected_shanghai_bytecode.to_lowercase());
}
