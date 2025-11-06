//! EVM Push Value
//!
//! This module provides the `PushValue` type for representing values to be pushed onto the EVM stack.

use crate::bytes_util::literal_gen;
use crate::evm_version::EVMVersion;
use alloy_primitives::{B256, hex};

/// Represents a value to be pushed onto the EVM stack
///
/// This type encapsulates a 32-byte value and provides methods to:
/// - Generate complete bytecode with optimal PUSH opcode (PUSH0-PUSH32)
/// - Extract just the hex data without any opcode
#[derive(Debug, Clone, PartialEq)]
pub struct PushValue {
    /// The 32-byte value to push
    pub value: B256,
}

impl PushValue {
    /// Creates a new PushValue from a B256 value
    pub fn new(value: B256) -> PushValue {
        PushValue { value }
    }

    /// Converts the PushValue to bytecode with the appropriate PUSH opcode
    ///
    /// This generates optimal bytecode by:
    /// - Trimming leading zeros from the value
    /// - Selecting the smallest PUSH opcode (PUSH0-PUSH32)
    /// - PUSH0 if the EVM version supports it and the value is zero
    /// - Encoding the result as a hex string
    ///
    /// Use this for normal macro/function bytecode generation.
    ///
    /// # Examples
    /// - Value `0x01` → `"6001"` (PUSH1 0x01)
    /// - Value `0x00` → `"5f"` (PUSH0, if supported by EVM version)
    /// - Value `0x123456...` → `"7f123456..."` (PUSH32)
    pub fn to_hex_with_opcode(&self, evm_version: &EVMVersion) -> String {
        literal_gen(evm_version, &self.value.0)
    }

    /// Returns the hex-encoded data without any PUSH opcode
    ///
    /// This trims leading zeros and returns just the hex data.
    /// Use this for padding operations or when you need just the raw data without leading zeros.
    ///
    /// # Examples
    /// - Value `0x0000...0001` → `"01"`
    /// - Value `0x0000...1234` → `"1234"`
    /// - Value `0x0000...0000` → `""` (empty string)
    pub fn to_hex_trimmed(&self) -> String {
        // Find the first non-zero byte to trim leading zeros
        let first_non_zero = self.value.0.iter().position(|&b| b != 0).unwrap_or(32);
        let trimmed_bytes = &self.value.0[first_non_zero..];
        hex::encode(trimmed_bytes)
    }

    /// Returns the full 32-byte hex-encoded data without any PUSH opcode
    ///
    /// This always returns the full 64-character hex string (32 bytes).
    /// Use this for code tables where the full 32-byte value must be preserved.
    ///
    /// # Examples
    /// - Value `0x0000...0001` → `"0000000000000000000000000000000000000000000000000000000000000001"`
    /// - Value `0x0000...1234` → `"0000000000000000000000000000000000000000000000000000000000001234"`
    /// - Value `0x0000...0000` → `"0000000000000000000000000000000000000000000000000000000000000000"`
    pub fn to_hex_full(&self) -> String {
        hex::encode(self.value.0)
    }

    /// Converts the full 32-byte value to hex string with PUSH32 opcode prefix (0x7f).
    ///
    /// Always generates a PUSH32 instruction (33 bytes total: 1 byte opcode + 32 bytes data),
    /// regardless of leading zeros. This is useful for operations that require exact 32-byte
    /// values on the stack, such as padding functions.
    ///
    /// # Examples
    /// - Value `0x0000...0001` → `"7f0000000000000000000000000000000000000000000000000000000000000001"`
    /// - Value `0x0000...1234` → `"7f0000000000000000000000000000000000000000000000000000000000001234"`
    /// - Value `0xFFFF...FFFF` → `"7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"`
    pub fn to_hex_full_with_opcode(&self) -> String {
        format!("7f{}", self.to_hex_full())
    }
}

/// Convert from a 32-byte array directly to PushValue
impl From<[u8; 32]> for PushValue {
    fn from(bytes: [u8; 32]) -> Self {
        PushValue { value: B256::from(bytes) }
    }
}

/// Convert from a reference to a 32-byte array to PushValue
impl From<&[u8; 32]> for PushValue {
    fn from(bytes: &[u8; 32]) -> Self {
        PushValue { value: B256::from(*bytes) }
    }
}

/// Convert from B256 to PushValue
impl From<B256> for PushValue {
    fn from(value: B256) -> Self {
        PushValue { value }
    }
}

/// Convert from a 4-byte array (function/error selector) to PushValue
///
/// This left-pads the 4 bytes to 32 bytes (zeros on the left, data on the right)
impl From<[u8; 4]> for PushValue {
    fn from(bytes: [u8; 4]) -> Self {
        let mut padded = [0u8; 32];
        padded[28..32].copy_from_slice(&bytes);
        PushValue { value: B256::from(padded) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evm_version::SupportedEVMVersions;
    use alloy_primitives::B256;

    #[test]
    fn test_to_hex_trimmed_single_byte() {
        let mut bytes = [0u8; 32];
        bytes[31] = 0x01;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_trimmed(), "01");
    }

    #[test]
    fn test_to_hex_trimmed_multiple_bytes() {
        let mut bytes = [0u8; 32];
        bytes[30] = 0x12;
        bytes[31] = 0x34;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_trimmed(), "1234");
    }

    #[test]
    fn test_to_hex_trimmed_all_zeros() {
        let bytes = [0u8; 32];
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_trimmed(), "");
    }

    #[test]
    fn test_to_hex_trimmed_full_32_bytes() {
        let bytes = [0xff; 32];
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_trimmed(), "f".repeat(64));
    }

    #[test]
    fn test_to_hex_trimmed_leading_zeros() {
        let mut bytes = [0u8; 32];
        bytes[28] = 0xde;
        bytes[29] = 0xad;
        bytes[30] = 0xbe;
        bytes[31] = 0xef;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_trimmed(), "deadbeef");
    }

    #[test]
    fn test_to_hex_full_single_byte() {
        let mut bytes = [0u8; 32];
        bytes[31] = 0x01;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_full(), "0000000000000000000000000000000000000000000000000000000000000001");
    }

    #[test]
    fn test_to_hex_full_multiple_bytes() {
        let mut bytes = [0u8; 32];
        bytes[30] = 0x12;
        bytes[31] = 0x34;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_full(), "0000000000000000000000000000000000000000000000000000000000001234");
    }

    #[test]
    fn test_to_hex_full_all_zeros() {
        let bytes = [0u8; 32];
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_full(), "0".repeat(64));
    }

    #[test]
    fn test_to_hex_full_all_ones() {
        let bytes = [0xff; 32];
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_full(), "f".repeat(64));
    }

    #[test]
    fn test_to_hex_full_preserves_leading_zeros() {
        let mut bytes = [0u8; 32];
        bytes[28] = 0xde;
        bytes[29] = 0xad;
        bytes[30] = 0xbe;
        bytes[31] = 0xef;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.to_hex_full(), "00000000000000000000000000000000000000000000000000000000deadbeef");
    }

    #[test]
    fn test_to_hex_with_opcode_push1() {
        let mut bytes = [0u8; 32];
        bytes[31] = 0x42;
        let pv = PushValue::from(bytes);
        let evm = EVMVersion::default();
        // PUSH1 = 0x60, so "6042" = PUSH1 0x42
        assert_eq!(pv.to_hex_with_opcode(&evm), "6042");
    }

    #[test]
    fn test_to_hex_with_opcode_push2() {
        let mut bytes = [0u8; 32];
        bytes[30] = 0x12;
        bytes[31] = 0x34;
        let pv = PushValue::from(bytes);
        let evm = EVMVersion::default();
        // PUSH2 = 0x61, so "611234" = PUSH2 0x1234
        assert_eq!(pv.to_hex_with_opcode(&evm), "611234");
    }

    #[test]
    fn test_to_hex_with_opcode_push0_shanghai() {
        let pv = PushValue::from(B256::ZERO);
        let evm = EVMVersion::new(SupportedEVMVersions::Shanghai);
        // PUSH0 = 0x5f for Shanghai and later
        assert_eq!(pv.to_hex_with_opcode(&evm), "5f");
    }

    #[test]
    fn test_to_hex_with_opcode_push1_zero_pre_shanghai() {
        let pv = PushValue::from(B256::ZERO);
        let evm = EVMVersion::new(SupportedEVMVersions::Paris);
        // Pre-Shanghai uses PUSH1 0x00 for zero
        assert_eq!(pv.to_hex_with_opcode(&evm), "6000");
    }

    #[test]
    fn test_to_hex_with_opcode_push4() {
        let mut bytes = [0u8; 32];
        bytes[28] = 0xde;
        bytes[29] = 0xad;
        bytes[30] = 0xbe;
        bytes[31] = 0xef;
        let pv = PushValue::from(bytes);
        let evm = EVMVersion::default();
        // PUSH4 = 0x63, so "63deadbeef" = PUSH4 0xdeadbeef
        assert_eq!(pv.to_hex_with_opcode(&evm), "63deadbeef");
    }

    #[test]
    fn test_to_hex_with_opcode_push32() {
        let pv = PushValue::from(B256::repeat_byte(0xff));
        let evm = EVMVersion::default();
        // PUSH32 = 0x7f
        assert_eq!(pv.to_hex_with_opcode(&evm), format!("7f{}", "f".repeat(64)));
    }

    #[test]
    fn test_from_u8_array_32() {
        let mut bytes = [0u8; 32];
        bytes[31] = 0x42;
        let pv = PushValue::from(bytes);
        assert_eq!(pv.value, B256::from(bytes));
    }

    #[test]
    fn test_from_ref_u8_array_32() {
        let mut bytes = [0u8; 32];
        bytes[31] = 0x42;
        let pv = PushValue::from(&bytes);
        assert_eq!(pv.value, B256::from(bytes));
    }

    #[test]
    fn test_from_b256() {
        let b256 = B256::repeat_byte(0xaa);
        let pv = PushValue::from(b256);
        assert_eq!(pv.value, b256);
    }

    #[test]
    fn test_from_u8_array_4_selector() {
        let selector = [0xde, 0xad, 0xbe, 0xef];
        let pv = PushValue::from(selector);

        // Should be left-padded to 32 bytes
        let mut expected = [0u8; 32];
        expected[28..32].copy_from_slice(&selector);
        assert_eq!(pv.value, B256::from(expected));
        assert_eq!(pv.to_hex_trimmed(), "deadbeef");
    }

    #[test]
    fn test_into_conversion() {
        let mut bytes = [0u8; 32];
        bytes[31] = 0xff;
        let pv: PushValue = bytes.into();
        assert_eq!(pv.value, B256::from(bytes));
    }

    #[test]
    fn test_from_b256_zero() {
        let pv = PushValue::from(B256::ZERO);
        assert_eq!(pv.value, B256::ZERO);
        assert_eq!(pv.to_hex_trimmed(), "");
    }
}
