use std::{str::FromStr, sync::Arc};

use huff_neo_core::Compiler;
use huff_neo_utils::file::file_provider::FileSystemFileProvider;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::remapper::Remapper;

#[test]
fn test_parses_foundry_aliased_imports() {
    // Set the current directory to the root of huff-neo-rs
    std::env::set_current_dir("../../").unwrap();

    // Create a remapper at the root level
    let remapper = Remapper::new("./");

    // Use an aliased import defined in foundry.toml for "resources" -> "examples"
    let mut import_bufs = vec![std::path::PathBuf::from_str("examples/erc20/ERC20.huff").unwrap()];

    // Remap import bufs with `remapper`. Panic on failure.
    import_bufs = import_bufs.into_iter().map(|p| std::path::PathBuf::from(remapper.remap(p.to_str().unwrap()).unwrap())).collect();

    // Fetch sources and unwrap errors
    let file_provider = FileSystemFileProvider {};
    let _ = Compiler::fetch_sources(import_bufs, Arc::new(file_provider)).into_iter().map(|r| r.unwrap()).collect::<Vec<Arc<FileSource>>>();
}

#[test]
#[should_panic]
fn test_invalid_imports_break() {
    let import_bufs = vec![std::path::PathBuf::from_str("unaliased/erc20/ERC20.huff").unwrap()];
    let file_provider = FileSystemFileProvider {};

    // Try to fetch sources
    let _ = Compiler::fetch_sources(import_bufs, Arc::new(file_provider)).into_iter().map(|r| r.unwrap()).collect::<Vec<Arc<FileSource>>>();
}
