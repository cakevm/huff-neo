//! Builtin Function Evaluation
//!
//! This module provides compile-time evaluation of Huff builtin functions.
//! These functions compute constant values that can be used in constant expressions.

use crate::ast::span::AstSpan;
use crate::error::{CodegenError, CodegenErrorKind};
use crate::prelude::{Argument, BuiltinFunctionArg, BuiltinFunctionCall, Contract, PushValue};
use alloy_primitives::{B256, hex, keccak256};

/// Creates a CodegenError for invalid arguments
fn invalid_arguments_error(msg: impl Into<String>, span: &AstSpan) -> CodegenError {
    CodegenError { kind: CodegenErrorKind::InvalidArguments(msg.into()), span: span.clone_box(), token: None }
}

/// Creates a CodegenError for invalid hex strings
fn invalid_hex_error(hex_str: impl Into<String>, span: &AstSpan) -> CodegenError {
    CodegenError { kind: CodegenErrorKind::InvalidHex(hex_str.into()), span: span.clone_box(), token: None }
}

/// Validates that the builtin function call has exactly the expected number of arguments
fn validate_arg_count(bf: &BuiltinFunctionCall, expected: usize, fn_name: &str) -> Result<(), CodegenError> {
    if bf.args.len() != expected {
        return Err(invalid_arguments_error(
            format!("Incorrect number of arguments passed to {}, should be {}: {}", fn_name, expected, bf.args.len()),
            &bf.span,
        ));
    }
    Ok(())
}

/// Extracts a single Argument from the first position of a builtin function call
fn extract_single_argument<'a>(bf: &'a BuiltinFunctionCall, fn_name: &str) -> Result<&'a Argument, CodegenError> {
    match bf.args.first() {
        Some(BuiltinFunctionArg::Argument(arg)) => Ok(arg),
        _ => Err(invalid_arguments_error(format!("Incorrect arguments type passed to {}", fn_name), &bf.span)),
    }
}

/// Formats hex string to have even length
fn format_even_bytes(hex: String) -> String {
    if hex.len() % 2 == 1 { format!("0{}", hex) } else { hex }
}

/// Evaluates __EVENT_HASH builtin function
pub fn eval_event_hash(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, "__EVENT_HASH")?;
    let first_arg = extract_single_argument(bf, "__EVENT_HASH")?;
    let hash = if let Some(event) = contract.events.iter().find(|e| first_arg.name.as_ref().unwrap().eq(&e.name)) {
        event.hash
    } else if let Some(s) = &first_arg.name {
        keccak256(s).0
    } else {
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingEventInterface(first_arg.name.as_ref().unwrap().to_string()),
            span: bf.span.clone_box(),
            token: None,
        });
    };
    Ok(PushValue::from(hash))
}

/// Evaluates __FUNC_SIG builtin function
pub fn eval_function_signature(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, "__FUNC_SIG")?;
    let first_arg = extract_single_argument(bf, "__FUNC_SIG")?;
    let selector = if let Some(func) = contract.functions.iter().find(|f| first_arg.name.as_ref().unwrap().eq(&f.name)) {
        func.signature
    } else if let Some(error) = contract.errors.iter().find(|e| first_arg.name.as_ref().unwrap().eq(&e.name)) {
        error.selector
    } else if let Some(s) = &first_arg.name {
        keccak256(s)[..4].try_into().unwrap()
    } else {
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingFunctionInterface(first_arg.name.as_ref().unwrap().to_string()),
            span: bf.span.clone_box(),
            token: None,
        });
    };

    // Left-pad the 4-byte selector to 32 bytes for B256
    let mut bytes = [0u8; 32];
    bytes[28..32].copy_from_slice(&selector);
    Ok(PushValue::from(bytes))
}

/// Evaluates __BYTES builtin function
pub fn eval_builtin_bytes(bf: &BuiltinFunctionCall) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, "__BYTES")?;
    let first_arg = match bf.args[0] {
        BuiltinFunctionArg::Argument(ref arg) => arg.name.clone().unwrap_or_default(),
        _ => {
            return Err(invalid_arguments_error("Invalid argument type passed to __BYTES", &bf.span));
        }
    };

    if first_arg.is_empty() {
        return Err(invalid_arguments_error("Empty string passed to __BYTES", &bf.span));
    }

    let bytes = first_arg.as_bytes();
    if bytes.len() > 32 {
        return Err(invalid_arguments_error("Encoded bytes length exceeds 32 bytes", &bf.span));
    }

    let mut bytes_array = [0u8; 32];
    bytes_array[32 - bytes.len()..].copy_from_slice(bytes);
    Ok(PushValue::from(bytes_array))
}

/// Direction for padding operations
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PadDirection {
    /// Left padding: zeros on the left, data on the right
    Left,
    /// Right padding: data on the left, zeros on the right
    Right,
}

impl std::fmt::Display for PadDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PadDirection::Left => write!(f, "__LEFTPAD"),
            PadDirection::Right => write!(f, "__RIGHTPAD"),
        }
    }
}

/// Evaluates __LEFTPAD or __RIGHTPAD builtin function
///
/// This is a simplified version that only handles literal arguments and nested builtin calls,
/// but not constant references. For constant references, the caller should resolve them first.
pub fn eval_builtin_pad_simple(bf: &BuiltinFunctionCall, direction: PadDirection) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, &direction.to_string())?;

    let first_arg = match &bf.args[0] {
        BuiltinFunctionArg::Argument(arg) => arg.name.clone().unwrap_or_default(),
        BuiltinFunctionArg::BuiltinFunctionCall(inner_call) => {
            // Recursively evaluate nested builtin calls
            let push_value = match inner_call.kind {
                crate::prelude::BuiltinFunctionKind::FunctionSignature => eval_function_signature(&Contract::default(), inner_call)?,
                crate::prelude::BuiltinFunctionKind::Bytes => eval_builtin_bytes(inner_call)?,
                crate::prelude::BuiltinFunctionKind::EventHash => eval_event_hash(&Contract::default(), inner_call)?,
                _ => {
                    return Err(invalid_arguments_error(format!("Invalid function call argument type passed to {direction}"), &bf.span));
                }
            };
            push_value.to_hex_trimmed()
        }
        _ => {
            return Err(invalid_arguments_error(format!("Invalid argument type passed to {direction}"), &bf.span));
        }
    };

    let hex = format_even_bytes(first_arg);

    // Parse hex string to bytes
    let hex_bytes = hex::decode(&hex).map_err(|_| invalid_hex_error(&hex, &bf.span))?;

    if hex_bytes.len() > 32 {
        return Err(invalid_arguments_error("Hex string exceeds 32 bytes", &bf.span));
    }

    let mut bytes_array = [0u8; 32];
    if direction == PadDirection::Left {
        // Left pad: zeros on the left, data on the right
        let start_idx = 32 - hex_bytes.len();
        bytes_array[start_idx..].copy_from_slice(&hex_bytes);
    } else {
        // Right pad: data on the left, zeros on the right
        bytes_array[..hex_bytes.len()].copy_from_slice(&hex_bytes);
    }

    Ok(PushValue::new(B256::from(bytes_array)))
}
