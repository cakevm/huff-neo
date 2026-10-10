use crate::{evm_version::EVMVersion, opcodes::Opcode};
use std::num::ParseIntError;

/// Convert a string slice to a `[u8; 32]`
/// Pads zeros to the left of significant bytes in the `[u8; 32]` slice.
/// i.e. 0xa57b becomes `[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/// 0, 0, 0, 0, 0, 165, 123]`
pub fn str_to_bytes32(s: &str) -> [u8; 32] {
    let s = format_even_bytes(String::from(s));

    let bytes: Vec<u8> = (0..s.len()).step_by(2).map(|c| u8::from_str_radix(&s[c..c + 2], 16).unwrap()).collect();

    let mut padded = [0u8; 32];

    for i in 32 - bytes.len()..32 {
        padded[i] = bytes[bytes.len() - (32 - i)];
    }

    padded
}

/// Convert a `[u8; 32]` to a bytes string.
pub fn bytes32_to_hex_string(bytes: &[u8; 32], prefixed: bool) -> String {
    let mut s = String::default();
    let start = bytes.iter().position(|b| *b != 0).unwrap_or(bytes.len() - 1);
    for b in &bytes[start..bytes.len()] {
        s = format!("{s}{:02x}", *b);
    }
    format!("{}{s}", if prefixed { "0x" } else { "" })
}

/// Wrapper to convert a hex string to a usize.
pub fn hex_to_usize(s: &str) -> Result<usize, ParseIntError> {
    usize::from_str_radix(s, 16)
}

/// Pad a hex string with n 0 bytes to the left. Will not pad a hex string that has a length
/// greater than or equal to `num_bytes * 2`
pub fn pad_n_bytes(hex: &str, num_bytes: usize) -> String {
    let mut hex = hex.to_owned();
    while hex.len() < num_bytes * 2 {
        hex = format!("0{hex}");
    }
    hex
}

/// Hex-encodes `value` as the smallest PUSHn (n >= 1) that holds it
fn push_min_width(value: usize) -> String {
    let width = ((usize::BITS - value.leading_zeros()) as usize).div_ceil(8).max(1);
    format!("{:02x}{:0digits$x}", 0x5f + width, value, digits = width * 2)
}

/// Builds the deployment bootstrap that copies the runtime code into memory and returns it
///
/// The runtime code is expected directly after the constructor body and the bootstrap itself.
/// Push widths grow with the values, so runtimes of 64 KiB and more (allowed from Amsterdam) are
/// encoded correctly. Returns the bootstrap hex string and its length in bytes.
pub fn deploy_bootstrap(runtime_len: usize, ctor_body_len: usize) -> (String, usize) {
    let size_push = push_min_width(runtime_len);
    // The runtime offset includes the bootstrap, whose length depends on the offset's push width
    let mut offset_width = 1;
    loop {
        // PUSHn <len>, DUP1, PUSHn <offset>, RETURNDATASIZE, CODECOPY, RETURNDATASIZE, RETURN
        let bootstrap_len = size_push.len() / 2 + 1 + (1 + offset_width) + 4;
        let offset_push = push_min_width(ctor_body_len + bootstrap_len);
        if offset_push.len() / 2 == 1 + offset_width {
            // 80 = DUP1; 3d = RETURNDATASIZE; 39 = CODECOPY; 3d = RETURNDATASIZE; f3 = RETURN
            return (format!("{size_push}80{offset_push}3d393df3"), bootstrap_len);
        }
        offset_width += 1;
    }
}

/// Pad odd-length byte string with a leading 0
pub fn format_even_bytes(hex: String) -> String {
    if hex.len() % 2 == 1 { format!("0{hex}") } else { hex }
}

/// Convert string slice to `Vec<u8>`, size not capped
pub fn str_to_vec(s: &str) -> Result<Vec<u8>, std::num::ParseIntError> {
    let bytes: Result<Vec<u8>, _> = (0..s.len()).step_by(2).map(|c| u8::from_str_radix(&s[c..c + 2], 16)).collect();
    bytes
}

/// Converts a value literal to its smallest equivalent `PUSHX` bytecode. Leading zeros are removed.
pub fn literal_gen(evm_version: &EVMVersion, l: &[u8; 32]) -> String {
    let hex_literal: String = bytes32_to_hex_string(l, false);
    match hex_literal.as_str() {
        "00" => format_push0(evm_version, hex_literal),
        _ => format_literal(hex_literal),
    }
}

/// Formats a `PUSH0` opcode if supported by the EVM version, otherwise falls back to `PUSH1 0x00`
fn format_push0(evm_version: &EVMVersion, hex_literal: String) -> String {
    if evm_version.has_push0() { Opcode::Push0.to_string() } else { format_literal(hex_literal) }
}

/// Converts a literal into its bytecode string representation
pub fn format_literal(hex_literal: String) -> String {
    format!("{:02x}{hex_literal}", 95 + hex_literal.len() / 2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deploy_bootstrap_push_widths() {
        // Empty runtime, no constructor: PUSH1 0x00, DUP1, PUSH1 0x09, ...
        assert_eq!(deploy_bootstrap(0, 0), ("60008060093d393df3".to_string(), 9));
        // Runtime needs PUSH2
        assert_eq!(deploy_bootstrap(300, 0), ("61012c80600a3d393df3".to_string(), 10));
        // Offset crosses 0xff only once the bootstrap's own length is included
        assert_eq!(deploy_bootstrap(1, 250), ("6001806101043d393df3".to_string(), 10));
        // Runtime of exactly 64 KiB needs PUSH3
        assert_eq!(deploy_bootstrap(0x10000, 0), ("6201000080600b3d393df3".to_string(), 11));
        // Offset crossing 0xffff widens to PUSH3 as well
        assert_eq!(deploy_bootstrap(1, 0xfff5), ("60018061ffff3d393df3".to_string(), 10));
        assert_eq!(deploy_bootstrap(1, 0xfff6), ("600180620100013d393df3".to_string(), 11));
    }
}
