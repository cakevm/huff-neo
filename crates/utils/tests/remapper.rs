use huff_neo_utils::file::remapper;
use tracing_subscriber::EnvFilter;

#[test]
fn test_generate_remappings() {
    let subscriber_builder = tracing_subscriber::fmt();
    let env_filter = EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into());
    if let Err(e) = subscriber_builder.with_env_filter(env_filter).try_init() {
        eprintln!("Failed to initialize tracing!\nError: {e:?}")
    }

    let remapper = remapper::Remapper::new("../../");
    assert_eq!(remapper.remappings.len(), 1);
    assert_eq!(remapper.remappings.get("examples/").unwrap(), "resources/");
}

#[test]
fn test_remappings_from_file() {
    let remapper = remapper::Remapper::new("./tests");
    assert_eq!(remapper.remappings.len(), 2);
    assert_eq!(remapper.remappings.get("@huffmate/").unwrap(), "lib/huffmate/src/");
    assert_eq!(remapper.remappings.get("@openzeppelin/").unwrap(), "lib/openzeppelin-contracts/contracts/");
}

#[test]
fn test_remappings_inferred_from_libs() {
    // A library with a src folder maps to it, one without maps to the library; explicit remappings win
    let root = std::env::temp_dir().join(format!("hnc-remapper-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("lib/with-src/src")).unwrap();
    std::fs::create_dir_all(root.join("lib/without-src")).unwrap();
    std::fs::create_dir_all(root.join("lib/explicit/src")).unwrap();
    std::fs::write(root.join("remappings.txt"), "explicit/=lib/explicit/contracts/\n").unwrap();

    let remapper = remapper::Remapper::new(root.to_str().unwrap());
    assert_eq!(remapper.remappings.get("with-src/").unwrap(), "lib/with-src/src/");
    assert_eq!(remapper.remappings.get("without-src/").unwrap(), "lib/without-src/");
    assert_eq!(remapper.remappings.get("explicit/").unwrap(), "lib/explicit/contracts/");
    std::fs::remove_dir_all(&root).unwrap();
}

#[test]
fn test_remap_uses_the_longest_prefix_once() {
    let remapper = remapper::Remapper {
        remappings: [("lib/".to_string(), "a/".to_string()), ("lib/sub/".to_string(), "b/".to_string())].into_iter().collect(),
        base_dir: "./".to_string(),
    };
    assert_eq!(remapper.remap("lib/sub/lib/x.huff").unwrap(), "./b/lib/x.huff");
    assert_eq!(remapper.remap("lib/x.huff").unwrap(), "./a/x.huff");
    assert_eq!(remapper.remap("other/x.huff"), None);
}
