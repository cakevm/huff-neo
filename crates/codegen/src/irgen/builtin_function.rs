use crate::Codegen;
use crate::irgen::constants::{constant_gen, evaluate_constant_value};
use alloy_primitives::{B256, hex, keccak256};
use huff_neo_utils::builtin_eval::{PadDirection, eval_builtin_bytes, eval_event_hash, eval_function_signature};
use huff_neo_utils::bytecode::{BytecodeRes, BytecodeSegments, Bytes, CircularCodeSizeIndices, Jump, Jumps};
use huff_neo_utils::bytes_util::{bytes32_to_hex_string, format_even_bytes, pad_n_bytes};
use huff_neo_utils::error::{CodegenError, CodegenErrorKind};
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::opcodes::Opcode;
use huff_neo_utils::prelude::{
    Argument, AstSpan, BuiltinFunctionArg, BuiltinFunctionCall, BuiltinFunctionKind, Contract, MacroDefinition, MacroInvocation, PushValue,
    TableDefinition,
};

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
        tracing::error!(
            target: "codegen",
            "Incorrect number of arguments passed to {}, should be {}: {}",
            fn_name,
            expected,
            bf.args.len()
        );
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

/// Validates that a string contains only hexadecimal characters
fn validate_hex_string(s: &str, span: &AstSpan) -> Result<(), CodegenError> {
    if s.chars().all(|c| c.is_ascii_hexdigit()) { Ok(()) } else { Err(invalid_hex_error(s, span)) }
}

/// Generates bytecode for all builtin function calls
///
/// This is the main dispatcher that handles all builtin function types:
/// - __codesize, __tablesize, __tablestart - Size and location utilities
/// - __FUNC_SIG, __EVENT_HASH, __ERROR - ABI encoding helpers
/// - __RIGHTPAD, __LEFTPAD - Byte padding (code tables only)
/// - __BYTES - Raw bytes insertion
/// - __VERBATIM - Direct hex injection
/// - __CODECOPY_DYN_ARG - Dynamic constructor arguments
/// - __ASSERT_PC - Compile-time program counter assertions
///
/// TODO: First step to refactor and split the function into smaller functions
#[allow(clippy::too_many_arguments)]
pub fn builtin_function_gen<'a>(
    evm_version: &EVMVersion,
    contract: &'a Contract,
    macro_def: &MacroDefinition,
    scope: &mut Vec<&'a MacroDefinition>,
    offset: &mut usize,
    mis: &mut Vec<(usize, MacroInvocation)>,
    table_instances: &mut Jumps,
    utilized_tables: &mut Vec<TableDefinition>,
    circular_codesize_invocations: &mut CircularCodeSizeIndices,
    starting_offset: usize,
    bytes: &mut BytecodeSegments,
    bf: &BuiltinFunctionCall,
) -> Result<(), CodegenError> {
    tracing::info!(target: "codegen", "RECURSE BYTECODE GOT BUILTIN FUNCTION CALL: {:?}", bf);
    match bf.kind {
        BuiltinFunctionKind::Codesize => {
            let (codesize_offset, push_bytes) =
                codesize(evm_version, contract, macro_def, scope, offset, mis, circular_codesize_invocations, bf)?;

            *offset += codesize_offset;
            // Check if this is a circular codesize placeholder
            let bytes_variant = if push_bytes == "cccc" { Bytes::CircularCodesizePlaceholder(push_bytes) } else { Bytes::Raw(push_bytes) };
            bytes.push_with_offset(starting_offset, bytes_variant);
        }
        BuiltinFunctionKind::Tablesize => {
            let (ir_table, push_bytes) = tablesize(contract, bf)?;

            if !utilized_tables.contains(&ir_table) {
                utilized_tables.push(ir_table);
            }

            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::Tablestart => {
            let first_arg = extract_single_argument(bf, "__tablestart")?;
            // Make sure the table exists
            if let Some(t) = contract.find_table_by_name(first_arg.name.as_ref().unwrap()) {
                tracing::debug!(target: "codegen", "Creating table instance for {} at offset {}", first_arg.name.as_ref().unwrap(), *offset);
                let scope_path: Vec<String> = if scope.len() > 1 {
                    let mut path: Vec<String> = scope[..scope.len() - 1].iter().map(|m| m.name.clone()).collect();
                    path.push(format!("{}_{}", scope.last().unwrap().name, *offset));
                    path
                } else {
                    scope.iter().map(|m| m.name.clone()).collect()
                };
                let scope_depth = scope.len().saturating_sub(1);
                table_instances.push(Jump {
                    label: first_arg.name.as_ref().unwrap().to_owned(),
                    bytecode_index: *offset,
                    span: bf.span.clone(),
                    scope_depth,
                    scope_path,
                });
                if !utilized_tables.contains(&t) {
                    utilized_tables.push(t);
                }

                bytes.push_with_offset(*offset, Bytes::JumpPlaceholder(format!("{}xxxx", Opcode::Push2)));
                *offset += 3;
            } else {
                tracing::error!(
                    target: "codegen",
                    "MISSING TABLE PASSED TO __tablestart \"{}\"",
                    first_arg.name.as_ref().unwrap()
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidMacroInvocation(first_arg.name.as_ref().unwrap().to_string()),
                    span: bf.span.clone_box(),
                    token: None,
                });
            }
        }
        BuiltinFunctionKind::FunctionSignature => {
            let push_value = eval_function_signature(contract, bf)?;
            let push_bytes = push_value.to_hex_with_opcode(evm_version);
            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::EventHash => {
            let push_value = eval_event_hash(contract, bf)?;
            let push_bytes = push_value.to_hex_with_opcode(evm_version);
            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::Error => {
            let push_value = error(contract, bf)?;
            let push_bytes = push_value.to_hex_with_opcode(evm_version);
            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::RightPad => {
            let push_value = builtin_pad(contract, bf, PadDirection::Right)?;
            let push_bytes = push_value.to_hex_with_opcode(evm_version);
            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::LeftPad => {
            return Err(invalid_arguments_error("LeftPad is not supported in a function or macro", &bf.span));
        }
        BuiltinFunctionKind::DynConstructorArg => {
            validate_arg_count(bf, 2, "__CODECOPY_DYN_ARG")?;

            let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
                return Err(invalid_arguments_error("Incorrect arguments type passed to __CODECOPY_DYN_ARG", &bf.span));
            };
            let BuiltinFunctionArg::Argument(ref second_arg) = bf.args[1] else {
                return Err(invalid_arguments_error("Incorrect arguments type passed to __CODECOPY_DYN_ARG", &bf.span));
            };

            let arg_index = first_arg.name.as_ref().unwrap();
            let dest_offset = second_arg.name.as_ref().unwrap();

            // Enforce that the arg index is 1 byte and that the dest offset is at max
            // 2 bytes.
            if arg_index.len() != 2 || dest_offset.len() > 4 {
                tracing::error!(
                    target = "codegen",
                    "Incorrect number of bytes in argument passed to __CODECOPY_DYN_ARG. Should be (1 byte, <= 2 bytes)"
                );
                return Err(invalid_arguments_error(
                    "Incorrect number of bytes in argument passed to __CODECOPY_DYN_ARG. Should be (1 byte, <= 2 bytes)",
                    &bf.span,
                ));
            }

            // Insert a 17 byte placeholder- will be filled when constructor args are added
            // to the end of the runtime code.
            // <len (2 bytes)> <dest_mem_ptr (2 bytes)> mstore
            // <len (2 bytes)> <contents_code_ptr (2 bytes)> <dest_mem_ptr + 0x20 (2 bytes)>
            // codecopy
            *offset += 17;
            bytes.push_with_offset(
                starting_offset,
                Bytes::DynConstructorArgPlaceholder(format!(
                    "{}{}{}",
                    "xx".repeat(14),
                    first_arg.name.as_ref().unwrap(),
                    pad_n_bytes(second_arg.name.as_ref().unwrap(), 2)
                )),
            );
        }
        BuiltinFunctionKind::Verbatim => {
            validate_arg_count(bf, 1, "__VERBATIM")?;

            let first_arg = extract_single_argument(bf, "__VERBATIM")?;
            let verbatim_str = first_arg.name.as_ref().unwrap();

            // check if verbatim was passed a hex string
            if let Err(e) = validate_hex_string(verbatim_str, &bf.span) {
                tracing::error!(
                    target: "codegen",
                    "INVALID HEX STRING PASSED TO __VERBATIM: \"{}\"",
                    verbatim_str
                );
                return Err(e);
            }

            tracing::debug!(target: "codegen", "INJECTING as verbatim: {}", verbatim_str);
            let hex = format_even_bytes(verbatim_str.clone());
            let push_bytes = hex.to_string();
            *offset += hex.len() / 2;

            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::Bytes => {
            let push_value = eval_builtin_bytes(bf)?;
            let push_bytes = push_value.to_hex_with_opcode(evm_version);
            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::AssertPc => {
            validate_arg_count(bf, 1, "__ASSERT_PC")?;

            // Extract the expected position from the argument
            let expected_position = match &bf.args[0] {
                BuiltinFunctionArg::Literal(lit) => {
                    // Parse the literal value (lit is a &[u8; 32])
                    let hex_str = bytes32_to_hex_string(lit, false);
                    usize::from_str_radix(&hex_str, 16).map_err(|_| {
                        invalid_arguments_error(format!("Invalid literal value passed to __ASSERT_PC: 0x{}", hex_str), &bf.span)
                    })?
                }
                BuiltinFunctionArg::Argument(arg) => {
                    // The parser may create an Argument with the hex value as the name
                    if let Some(name) = &arg.name {
                        // Try to parse as hex
                        usize::from_str_radix(name, 16)
                            .map_err(|_| invalid_arguments_error(format!("Invalid hex value passed to __ASSERT_PC: {}", name), &bf.span))?
                    } else {
                        return Err(invalid_arguments_error("Argument has no name", &bf.span));
                    }
                }
                BuiltinFunctionArg::Constant(name, span) => {
                    // Evaluate constant directly to numeric value without generating bytecode
                    evaluate_constant_value(name, contract, span)?
                }
                _ => {
                    return Err(invalid_arguments_error("Expected literal or constant value as argument to __ASSERT_PC", &bf.span));
                }
            };

            // Check if the current position matches the expected position
            if starting_offset != expected_position {
                tracing::error!(
                    target: "codegen",
                    "PC assertion failed: expected 0x{:x}, got 0x{:x}",
                    expected_position,
                    starting_offset
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::AssertPcFailed(expected_position, starting_offset),
                    span: bf.span.clone_box(),
                    token: None,
                });
            }

            tracing::debug!(
                target: "codegen",
                "PC assertion passed: position is 0x{:x} as expected",
                starting_offset
            );

            // No bytecode is generated - this is a pure compile-time check
        }
    }
    Ok(())
}

/// Generates bytecode for the __codesize builtin function
///
/// Returns a tuple of (byte_offset, bytecode_string) containing the size of the specified macro.
/// Handles circular references by inserting a placeholder ("cccc") that's filled in later.
#[allow(clippy::too_many_arguments)]
fn codesize<'a>(
    evm_version: &EVMVersion,
    contract: &'a Contract,
    macro_def: &MacroDefinition,
    scope: &mut Vec<&'a MacroDefinition>,
    offset: &mut usize,
    mis: &mut Vec<(usize, MacroInvocation)>,
    circular_codesize_invocations: &mut CircularCodeSizeIndices,
    bf: &BuiltinFunctionCall,
) -> Result<(usize, String), CodegenError> {
    let first_arg = extract_single_argument(bf, "__codesize")?;
    let ir_macro = if let Some(m) = contract.find_macro_by_name(first_arg.name.as_ref().unwrap()) {
        m
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING MACRO PASSED TO __codesize \"{}\"",
            first_arg.name.as_ref().unwrap()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingMacroDefinition(first_arg.name.as_ref().unwrap().to_string() /* yuck */),
            span: bf.span.clone_box(),
            token: None,
        });
    };

    // Get the name of the macro being passed to __codesize
    let codesize_arg = first_arg.name.as_ref().unwrap();
    let is_previous_parent = scope.iter().any(|def| def.name == *codesize_arg);

    // Special case:
    // If the macro provided to __codesize is the current macro, we need to avoid a
    // circular reference If this is the case we will store a
    // placeholder inside the bytecode and fill it in later when
    // we have adequate information about the macros eventual size.
    // We also need to avoid if the codesize arg is any of the previous macros to
    // avoid a circular reference
    let (codesize_offset, push_bytes) = if is_previous_parent || macro_def.name.eq(codesize_arg) {
        tracing::debug!(target: "codegen", "CIRCULAR CODESIZE INVOCATION DETECTED INJECTING PLACEHOLDER | macro: {}", ir_macro.name);

        // Save the invocation for later
        circular_codesize_invocations.insert((codesize_arg.to_string(), *offset));

        // Progress offset by placeholder size
        (2, "cccc".to_string())
    } else {
        // We will still need to recurse to get accurate values
        let res: BytecodeRes = match Codegen::macro_to_bytecode(
            evm_version,
            ir_macro,
            contract,
            scope,
            *offset,
            mis,
            ir_macro.name.eq("CONSTRUCTOR"),
            Some(circular_codesize_invocations),
        ) {
            Ok(r) => r,
            Err(e) => {
                tracing::error!(
                    target: "codegen",
                    "FAILED TO RECURSE INTO MACRO \"{}\"",
                    ir_macro.name
                );
                return Err(e);
            }
        };

        let size = format_even_bytes(format!("{:02x}", (res.bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>() / 2)));
        let push_bytes = format!("{:02x}{size}", 95 + size.len() / 2);
        let offset = push_bytes.len() / 2;
        (offset, push_bytes)
    };
    Ok((codesize_offset, push_bytes))
}

/// Generates a PushValue for the __ERROR builtin function
///
/// Returns a PushValue containing the error selector.
/// For error definitions: selector at start, right-padded with zeros (PUSH32 equivalent).
/// For raw signatures: left-padded selector (PUSH4 equivalent).
pub fn error(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, "__ERROR")?;
    let first_arg = extract_single_argument(bf, "__ERROR")?;

    let mut bytes = [0u8; 32];
    if let Some(error) = contract.errors.iter().find(|e| first_arg.name.as_ref().unwrap().eq(&e.name)) {
        // Put selector at start, right-padded with zeros (rest is already zero)
        bytes[0..4].copy_from_slice(&error.selector);
    } else if let Some(s) = &first_arg.name {
        let selector: [u8; 4] = keccak256(s)[..4].try_into().unwrap();
        // Left-pad the selector (put at end)
        bytes[28..32].copy_from_slice(&selector);
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING ERROR DEFINITION PASSED TO __ERROR: \"{}\"",
            first_arg.name.as_ref().unwrap()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingErrorDefinition(first_arg.name.as_ref().unwrap().to_string()),
            span: bf.span.clone_box(),
            token: None,
        });
    };

    Ok(PushValue::from(bytes))
}

/// Generates bytecode for the __tablesize builtin function
///
/// Returns a tuple of (TableDefinition, bytecode_string) with the size of the specified jump table.
/// The bytecode is a PUSH instruction with the table size in bytes.
pub fn tablesize(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<(TableDefinition, String), CodegenError> {
    let first_arg = extract_single_argument(bf, "__tablesize")?;
    let ir_table = if let Some(t) = contract.find_table_by_name(first_arg.name.as_ref().unwrap()) {
        t
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING TABLE PASSED TO __tablesize \"{}\"",
            first_arg.name.as_ref().unwrap()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidMacroInvocation(first_arg.name.as_ref().unwrap().to_string() /* yuck */),
            span: bf.span.clone_box(),
            token: None,
        });
    };
    let Some(table_size) = ir_table.size else {
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingTableSize(ir_table.name.clone()),
            span: bf.span.clone_box(),
            token: None,
        });
    };
    let size = bytes32_to_hex_string(&table_size, false);
    let push_bytes = format!("{:02x}{size}", 95 + size.len() / 2);
    Ok((ir_table, push_bytes))
}

/// Generates a PushValue for left or right padding a byte string to 32 bytes
pub fn builtin_pad(contract: &Contract, bf: &BuiltinFunctionCall, direction: PadDirection) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, &direction.to_string())?;
    let first_arg = match &bf.args[0] {
        BuiltinFunctionArg::Argument(arg) => arg.name.clone().unwrap_or_default(),
        BuiltinFunctionArg::BuiltinFunctionCall(inner_call) => match inner_call.kind {
            BuiltinFunctionKind::FunctionSignature => eval_function_signature(contract, inner_call)?.to_hex_trimmed(),
            BuiltinFunctionKind::Bytes => eval_builtin_bytes(inner_call)?.to_hex_trimmed(),
            BuiltinFunctionKind::EventHash => eval_event_hash(contract, inner_call)?.to_hex_trimmed(),
            _ => {
                tracing::error!(target: "codegen", "Invalid function call argument type passed to {direction}");
                return Err(invalid_arguments_error(format!("Invalid argument type passed to {direction}"), &bf.span));
            }
        },
        BuiltinFunctionArg::Constant(name, span) => {
            constant_gen(name, contract, span)?.map(|pv| pv.to_hex_trimmed()).unwrap_or_default() // __NOOP returns empty string
        }
        _ => {
            tracing::error!(target: "codegen", "Invalid argument type passed to {direction}");
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
