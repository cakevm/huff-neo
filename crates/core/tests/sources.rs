use std::{path::PathBuf, sync::Arc};

use huff_neo_core::Compiler;
use huff_neo_utils::file::file_provider::FileSystemFileProvider;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_fetch_sources() {
    let file_provider = Arc::new(FileSystemFileProvider {});
    let file_sources: Vec<Arc<FileSource>> = Compiler::fetch_sources(
        vec![
            PathBuf::from("../../resources/erc20/ERC20.huff".to_string()),
            PathBuf::from("../../resources/utils/HashMap.huff".to_string()),
            PathBuf::from("../../resources/utils/Ownable.huff".to_string()),
        ],
        file_provider,
    )
    .iter()
    .map(|p| p.clone().unwrap())
    .collect();
    assert_eq!(file_sources.len(), 3);
    assert_eq!(file_sources[0].path, "../../resources/erc20/ERC20.huff".to_string());
    assert_eq!(file_sources[1].path, "../../resources/utils/HashMap.huff".to_string());
    assert_eq!(file_sources[2].path, "../../resources/utils/Ownable.huff".to_string());
}

#[test]
fn test_fetch_invalid_sources() {
    let paths = vec![
        PathBuf::from("../../resources/erc20/non_existant.huff".to_string()),
        PathBuf::from("../../resources/erc20/non_huff.txt".to_string()),
        PathBuf::from("../../resources/erc20/random/Address.huff".to_string()),
        PathBuf::from("../../resources/erc20/random/".to_string()),
        PathBuf::from("../../resources/erc20/utils/".to_string()),
    ];
    let file_provider = FileSystemFileProvider {};
    let file_sources: Vec<Result<Arc<FileSource>, CompilerError>> = Compiler::fetch_sources(paths.clone(), Arc::new(file_provider));
    for (i, e) in file_sources.iter().enumerate() {
        let file_loc = String::from(paths[i].to_string_lossy());
        assert_eq!(e.clone().err().unwrap(), CompilerError::FileUnpackError(UnpackError::MissingFile(file_loc)))
    }
}
