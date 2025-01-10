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
    assert_eq!(remapper.remappings.get("examples/").unwrap(), "huff-examples/");
}

#[test]
fn test_remappings_from_file() {
    let remapper = remapper::Remapper::new("./tests");
    assert_eq!(remapper.remappings.len(), 2);
    assert_eq!(remapper.remappings.get("@huffmate/").unwrap(), "lib/huffmate/src/");
    assert_eq!(remapper.remappings.get("@openzeppelin/").unwrap(), "lib/openzeppelin-contracts/contracts/");
}
