use huff_neo_utils::bytes_util::{bytes32_to_hex_string, literal_gen, str_to_bytes32};
use huff_neo_utils::evm_version::{EVMVersion, SupportedEVMVersions};

#[test]
fn test_converts_literal_to_hex_string() {
    let sources = ["00", "01", "1000", "010101", "a57b", "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"];

    for source in sources {
        assert_eq!(format!("0x{source}"), bytes32_to_hex_string(&str_to_bytes32(source), true));
    }
}

#[test]
fn test_literal_gen() {
    // push1 = 0x60
    let literal = literal_gen(&EVMVersion::new(SupportedEVMVersions::Paris), &str_to_bytes32("00"));
    assert_eq!(literal, "6000");

    // push0 = 0x5f
    let literal = literal_gen(&EVMVersion::new(SupportedEVMVersions::Cancun), &str_to_bytes32("00"));
    assert_eq!(literal, "5f");

    // push0 = 0x5f
    let literal = literal_gen(&EVMVersion::new(SupportedEVMVersions::Cancun), &str_to_bytes32("0000"));
    assert_eq!(literal, "5f");

    // push8 = 0x61
    let literal = literal_gen(&EVMVersion::new(SupportedEVMVersions::Cancun), &str_to_bytes32("0000000000001234"));
    assert_eq!(literal, "611234");
}
