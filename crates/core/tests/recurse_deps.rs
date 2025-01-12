use huff_neo_core::Compiler;
use huff_neo_utils::file::file_provider::FileSystemFileProvider;
use huff_neo_utils::file::{file_source, remapper};
use std::collections::HashSet;
use std::{path::PathBuf, sync::Arc};

#[test]
fn test_recursing_fs_dependencies() {
    let file_provider = Arc::new(FileSystemFileProvider {});
    let file_sources: Vec<Arc<file_source::FileSource>> =
        Compiler::fetch_sources(vec![PathBuf::from("../../resources/erc20/ERC20.huff".to_string())], file_provider.clone())
            .iter()
            .map(|p| p.clone().unwrap())
            .collect();
    assert_eq!(file_sources.len(), 1);
    let erc20_file_source = file_sources[0].clone();
    let res = Compiler::recurse_deps(Arc::clone(&erc20_file_source), &remapper::Remapper::new("./"), file_provider, HashSet::new());
    let full_erc20_file_source = res.unwrap();
    assert_eq!(full_erc20_file_source.dependencies.len(), 2);
    for dep in full_erc20_file_source.dependencies.iter() {
        assert!(dep.source.is_some());
        assert_eq!(dep.dependencies.len(), 0);
    }
}

#[test]
fn test_recursing_external_dependencies() {
    let file_provider = Arc::new(FileSystemFileProvider {});
    let file_sources: Vec<Arc<file_source::FileSource>> =
        Compiler::fetch_sources(vec![PathBuf::from("../../resources/erc20/ERC20.huff".to_string())], file_provider.clone())
            .iter()
            .map(|p| p.clone().unwrap())
            .collect();
    assert_eq!(file_sources.len(), 1);
    let erc20_file_source = file_sources[0].clone();
    let res = Compiler::recurse_deps(Arc::clone(&erc20_file_source), &remapper::Remapper::new("./"), file_provider, HashSet::new());
    let full_erc20_file_source = res.unwrap();

    assert_eq!(full_erc20_file_source.dependencies.len(), 2);
    for dep in full_erc20_file_source.dependencies.iter() {
        assert!(dep.source.is_some());
        assert_eq!(dep.dependencies.len(), 0);
    }
}
