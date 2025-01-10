use huff_neo_utils::bytes_util::{bytes32_to_string, str_to_bytes32};

#[test]
fn converts_literal_to_hex_string() {
    let sources = ["00", "01", "1000", "010101", "a57b", "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"];

    for source in sources {
        assert_eq!(format!("0x{source}"), bytes32_to_string(&str_to_bytes32(source), true));
    }
}
