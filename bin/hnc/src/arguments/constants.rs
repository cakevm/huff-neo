use huff_neo_utils::bytecode::Bytes;
use std::collections::BTreeMap;
use thiserror::Error;

/// Errors that can occur when parsing constant overrides
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ConstantParseError {
    /// Missing '=' separator in the override argument
    #[error("Missing '=' in constant override: {0}")]
    MissingSeparator(String),
    /// Invalid constant identifier (must start with letter/underscore, followed by alphanumeric/underscore)
    #[error("Invalid constant name '{0}': must start with letter or underscore, followed by alphanumeric or underscore")]
    InvalidIdentifier(String),
    /// Value doesn't start with '0x' prefix
    #[error("Value must start with '0x': {0}")]
    MissingHexPrefix(String),
    /// Value contains non-hexadecimal characters or is empty after '0x'
    #[error("Invalid hex value '{0}': must contain only hex digits")]
    InvalidHexValue(String),
}

/// Parse constant override arguments in the format NAME=0xVALUE
///
/// Validates that:
/// - Each argument contains exactly one '='
/// - Name starts with alphabetic or underscore, followed by alphanumeric or underscore
/// - Value starts with '0x' and contains only hex digits
///
/// Returns an error if any validation fails.
pub fn parse_constant_overrides(constants: &[String]) -> Result<BTreeMap<&str, Bytes>, ConstantParseError> {
    constants
        .iter()
        .map(|c| {
            // Split on '=' to get key=value pair
            let (key, value) = c.split_once('=').ok_or_else(|| ConstantParseError::MissingSeparator(c.clone()))?;

            // Validate identifier is a valid Huff constant name
            if !is_valid_identifier(key) {
                return Err(ConstantParseError::InvalidIdentifier(key.to_string()));
            }

            // Handle true/false keywords, otherwise parse hex value
            let hex_value = if value == "true" {
                "01"
            } else if value == "false" {
                "00"
            } else {
                // Strip '0x' prefix from hex value
                let hex = value.strip_prefix("0x").ok_or_else(|| ConstantParseError::MissingHexPrefix(value.to_string()))?;

                // Validate hex string is non-empty and contains only valid hex digits
                if hex.is_empty() || !hex.chars().all(|c| c.is_ascii_hexdigit()) {
                    return Err(ConstantParseError::InvalidHexValue(value.to_string()));
                }
                hex
            };

            Ok((key, Bytes::Raw(hex_value.to_string())))
        })
        .collect()
}

/// Validate that a string is a valid Huff constant identifier
///
/// Rules:
/// - Must not be empty
/// - First character must be alphabetic or underscore
/// - Remaining characters must be alphanumeric or underscore
fn is_valid_identifier(s: &str) -> bool {
    !s.is_empty()
        && s.chars().enumerate().all(|(i, c)| if i == 0 { c.is_alphabetic() || c == '_' } else { c.is_alphanumeric() || c == '_' })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_identifiers() {
        assert!(is_valid_identifier("FOO"));
        assert!(is_valid_identifier("_FOO"));
        assert!(is_valid_identifier("FOO123"));
        assert!(is_valid_identifier("FOO_BAR"));
        assert!(is_valid_identifier("_"));
        assert!(is_valid_identifier("a"));
    }

    #[test]
    fn test_invalid_identifiers() {
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("123FOO"));
        assert!(!is_valid_identifier("FOO-BAR"));
        assert!(!is_valid_identifier("FOO BAR"));
        assert!(!is_valid_identifier("123"));
    }

    #[test]
    fn test_parse_valid_constant() {
        let input = vec!["TEST=0xff".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(result.is_ok());
        let map = result.unwrap();
        assert_eq!(map.len(), 1);
        assert_eq!(map.get("TEST").unwrap().as_str(), "ff");
    }

    #[test]
    fn test_parse_multiple_constants() {
        let input = vec!["FOO=0x42".to_string(), "BAR_123=0xabcd".to_string(), "_PRIVATE=0x00".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(result.is_ok());
        let map = result.unwrap();
        assert_eq!(map.len(), 3);
        assert_eq!(map.get("FOO").unwrap().as_str(), "42");
        assert_eq!(map.get("BAR_123").unwrap().as_str(), "abcd");
        assert_eq!(map.get("_PRIVATE").unwrap().as_str(), "00");
    }

    #[test]
    fn test_parse_missing_separator() {
        let input = vec!["TEST0xff".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(matches!(result, Err(ConstantParseError::MissingSeparator(_))));
    }

    #[test]
    fn test_parse_invalid_identifier_starts_with_number() {
        let input = vec!["123TEST=0xff".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(matches!(result, Err(ConstantParseError::InvalidIdentifier(_))));
    }

    #[test]
    fn test_parse_invalid_identifier_contains_hyphen() {
        let input = vec!["TEST-FOO=0xff".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(matches!(result, Err(ConstantParseError::InvalidIdentifier(_))));
    }

    #[test]
    fn test_parse_missing_hex_prefix() {
        let input = vec!["TEST=ff".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(matches!(result, Err(ConstantParseError::MissingHexPrefix(_))));
    }

    #[test]
    fn test_parse_empty_hex_value() {
        let input = vec!["TEST=0x".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(matches!(result, Err(ConstantParseError::InvalidHexValue(_))));
    }

    #[test]
    fn test_parse_invalid_hex_characters() {
        let input = vec!["TEST=0xGG".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(matches!(result, Err(ConstantParseError::InvalidHexValue(_))));
    }

    #[test]
    fn test_parse_bool_true_converts_to_hex_01() {
        let input = vec!["DEBUG=true".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(result.is_ok());
        let map = result.unwrap();
        assert_eq!(map.get("DEBUG").unwrap().as_str(), "01");
    }

    #[test]
    fn test_parse_bool_false_converts_to_hex_00() {
        let input = vec!["DEBUG=false".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(result.is_ok());
        let map = result.unwrap();
        assert_eq!(map.get("DEBUG").unwrap().as_str(), "00");
    }

    #[test]
    fn test_parse_mixed_bool_and_hex_values() {
        let input = vec!["ENABLED=true".to_string(), "DISABLED=false".to_string(), "VALUE=0xff".to_string()];
        let result = parse_constant_overrides(&input);
        assert!(result.is_ok());
        let map = result.unwrap();
        assert_eq!(map.get("ENABLED").unwrap().as_str(), "01");
        assert_eq!(map.get("DISABLED").unwrap().as_str(), "00");
        assert_eq!(map.get("VALUE").unwrap().as_str(), "ff");
    }
}
