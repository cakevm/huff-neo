use crate::Codegen;
use crate::irgen::constants::{constant_gen, evaluate_constant_value, lookup_constant};
use alloy_primitives::{B256, hex, keccak256};
use huff_neo_utils::builtin_eval::{PadDirection, eval_builtin_bytes, eval_event_hash, eval_function_signature};
use huff_neo_utils::bytecode::{
    AssertPcPlaceholderData, BytecodeRes, BytecodeSegments, Bytes, CircularCodeSizeIndices, CircularCodesizePlaceholderData,
    DynConstructorArgPlaceholderData, Jump, JumpPlaceholderData, Jumps, PushOpcode,
};
use huff_neo_utils::bytes_util::{bytes32_to_hex_string, format_even_bytes, pad_n_bytes};
use huff_neo_utils::error::{CodegenError, CodegenErrorKind};
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::prelude::{
    AstSpan, BuiltinFunctionArg, BuiltinFunctionCall, BuiltinFunctionKind, ConstVal, Contract, MacroDefinition, PushValue, TableDefinition,
    TableKind,
};
use huff_neo_utils::scope::ScopeManager;
use std::collections::BTreeMap;

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
    scope_mgr: &mut ScopeManager<'a>,
    offset: &mut usize,
    table_instances: &mut Jumps,
    utilized_tables: &mut Vec<TableDefinition>,
    embedded_tables: &mut BTreeMap<String, usize>,
    circular_codesize_invocations: &mut CircularCodeSizeIndices,
    starting_offset: usize,
    bytes: &mut BytecodeSegments,
    bf: &BuiltinFunctionCall,
    relax_jumps: bool,
) -> Result<(), CodegenError> {
    tracing::info!(target: "codegen", "RECURSE BYTECODE GOT BUILTIN FUNCTION CALL: {:?}", bf);
    match bf.kind {
        BuiltinFunctionKind::Codesize => {
            let (codesize_offset, bytes_variant) =
                codesize(evm_version, contract, macro_def, scope_mgr, offset, circular_codesize_invocations, bf, relax_jumps)?;

            *offset += codesize_offset;
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
            validate_arg_count(bf, 1, "__tablestart")?;

            let table_name = match &bf.args[0] {
                BuiltinFunctionArg::Identifier(name, _) => name,
                _ => {
                    return Err(invalid_arguments_error("Expected identifier as argument to __tablestart", &bf.span));
                }
            };

            // Make sure the table exists
            if let Some(t) = contract.find_table_by_name(table_name) {
                // Check if the table was embedded
                if let Some(&embedded_offset) = embedded_tables.get(table_name) {
                    // Table was embedded - push the actual embedding offset directly
                    let hex_offset = format!("{embedded_offset:04x}");
                    let push_bytes = format!("61{hex_offset}"); // PUSH2

                    tracing::debug!(
                        target: "codegen",
                        "__tablestart for embedded table \"{}\" at embedded offset 0x{:04x}",
                        table_name,
                        embedded_offset
                    );

                    bytes.push_with_offset(*offset, Bytes::Raw(push_bytes));
                    *offset += 3;
                } else {
                    // Table not embedded - use placeholder for end-placement
                    tracing::debug!(target: "codegen", "Creating table instance for {} at offset {}", table_name, *offset);
                    let scope_id = scope_mgr.current_scope();
                    table_instances.push(Jump { label: table_name.to_owned(), bytecode_index: *offset, span: bf.span.clone(), scope_id });

                    bytes.push_with_offset(
                        *offset,
                        Bytes::JumpPlaceholder(JumpPlaceholderData::new(table_name.to_owned(), PushOpcode::Push2, String::new())),
                    );
                    *offset += 3;
                }

                if !utilized_tables.contains(&t) {
                    utilized_tables.push(t);
                }
            } else {
                tracing::error!(
                    target: "codegen",
                    "MISSING TABLE PASSED TO __tablestart \"{}\"",
                    table_name
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidMacroInvocation(table_name.to_string()),
                    span: bf.span.clone_box(),
                    token: None,
                });
            }
        }
        BuiltinFunctionKind::EmbedTable => {
            validate_arg_count(bf, 1, "__EMBED_TABLE")?;

            let table_name = match &bf.args[0] {
                BuiltinFunctionArg::Identifier(name, _) => name,
                _ => {
                    return Err(invalid_arguments_error("Expected identifier as argument to __EMBED_TABLE", &bf.span));
                }
            };

            // Find the table definition
            let table_def = if let Some(t) = contract.find_table_by_name(table_name) {
                t
            } else {
                tracing::error!(
                    target: "codegen",
                    "MISSING TABLE PASSED TO __EMBED_TABLE \"{}\"",
                    table_name
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidMacroInvocation(table_name.to_string()),
                    span: bf.span.clone_box(),
                    token: None,
                });
            };

            // Check if table was already embedded (we only allow one embedding per table)
            if embedded_tables.contains_key(table_name) {
                return Err(CodegenError {
                    kind: CodegenErrorKind::DuplicateTableEmbedding(table_name.to_string()),
                    span: bf.span.clone_box(),
                    token: None,
                });
            }

            // Track the embedding position (for __tablestart)
            embedded_tables.insert(table_name.to_string(), *offset);

            // Add table to utilized tables if not already there
            if !utilized_tables.contains(&table_def) {
                utilized_tables.push(table_def.clone());
            }

            // Generate the table bytecode inline
            let table_code = match table_def.kind {
                TableKind::CodeTable => {
                    // Use the existing gen_table_bytecode_builtin function for code tables
                    Codegen::gen_table_bytecode_builtin(contract, &table_def)?
                }
                TableKind::JumpTable | TableKind::JumpTablePacked => {
                    // Generate jump table bytecode inline
                    // We need access to label_indices from BytecodeRes, but we don't have it here
                    // For now we only support code tables with __EMBED_TABLE
                    return Err(CodegenError {
                        kind: CodegenErrorKind::InvalidArguments(
                            "Jump tables cannot be embedded inline with __EMBED_TABLE yet. Only code tables are supported.".to_string(),
                        ),
                        span: bf.span.clone_box(),
                        token: None,
                    });
                }
            };

            // Insert the table bytecode at the current position
            let table_size_bytes = table_code.len() / 2;
            *offset += table_size_bytes;
            bytes.push_with_offset(starting_offset, Bytes::Raw(table_code));

            tracing::debug!(
                target: "codegen",
                "Embedded table \"{}\" at offset {}, size {} bytes",
                table_name,
                starting_offset,
                table_size_bytes
            );
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
            // For padding, always use PUSH32 with full 32 bytes (no optimization)
            let push_bytes = push_value.to_hex_full_with_opcode();
            *offset += 33; // PUSH32 opcode (1 byte) + 32 bytes of data
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::LeftPad => {
            // __LEFTPAD is only available in constant definitions and code tables, not in macro bodies
            return Err(invalid_arguments_error("__LEFTPAD is only available in constant definitions and code tables", &bf.span));
        }
        BuiltinFunctionKind::DynConstructorArg => {
            validate_arg_count(bf, 2, "__CODECOPY_DYN_ARG")?;

            let arg_index = match &bf.args[0] {
                BuiltinFunctionArg::HexLiteral(hex, _) => hex,
                _ => {
                    return Err(invalid_arguments_error("Expected hex literal as first argument to __CODECOPY_DYN_ARG", &bf.span));
                }
            };

            let dest_offset = match &bf.args[1] {
                BuiltinFunctionArg::HexLiteral(hex, _) => hex,
                _ => {
                    return Err(invalid_arguments_error("Expected hex literal as second argument to __CODECOPY_DYN_ARG", &bf.span));
                }
            };

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
                Bytes::DynConstructorArgPlaceholder(DynConstructorArgPlaceholderData::new(
                    arg_index.to_string(),
                    pad_n_bytes(dest_offset, 2),
                )),
            );
        }
        BuiltinFunctionKind::Verbatim => {
            validate_arg_count(bf, 1, "__VERBATIM")?;

            let verbatim_str = match &bf.args[0] {
                BuiltinFunctionArg::HexLiteral(hex, _) => hex.clone(),
                BuiltinFunctionArg::Constant(name, span) => {
                    // Constant reference - resolve it
                    constant_gen(name, contract, span)?.map(|pv| pv.to_hex_trimmed()).unwrap_or_default()
                }
                _ => {
                    return Err(invalid_arguments_error("Expected hex literal or constant reference as argument to __VERBATIM", &bf.span));
                }
            };

            // check if verbatim was passed a hex string
            if let Err(e) = validate_hex_string(&verbatim_str, &bf.span) {
                tracing::error!(
                    target: "codegen",
                    "INVALID HEX STRING PASSED TO __VERBATIM: \"{}\"",
                    verbatim_str
                );
                return Err(e);
            }

            tracing::debug!(target: "codegen", "INJECTING as verbatim: {}", verbatim_str);
            let hex = format_even_bytes(verbatim_str);
            let push_bytes = hex.to_string();
            *offset += hex.len() / 2;

            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::Bytes => {
            validate_arg_count(bf, 1, "__BYTES")?;

            let string_value = match &bf.args[0] {
                BuiltinFunctionArg::StringLiteral(s, _) => s.clone(),
                BuiltinFunctionArg::Constant(name, span) => {
                    // Constant reference - check what type it is
                    let constant = lookup_constant(name, contract, span)?;
                    match &constant.value {
                        ConstVal::String(s) => s.clone(),
                        ConstVal::BuiltinFunctionCall(inner_bf) if matches!(inner_bf.kind, BuiltinFunctionKind::Bytes) => {
                            // Allow chained __BYTES constants
                            eval_builtin_bytes(inner_bf)?.to_hex_trimmed()
                        }
                        _ => {
                            return Err(invalid_arguments_error(
                                format!("__BYTES requires a string literal or string constant. Constant [{}] is not a string.", name),
                                &bf.span,
                            ));
                        }
                    }
                }
                _ => {
                    return Err(invalid_arguments_error("Expected string literal or constant reference as argument to __BYTES", &bf.span));
                }
            };

            if string_value.is_empty() {
                return Err(invalid_arguments_error("Empty string passed to __BYTES", &bf.span));
            }

            // Convert string to bytes and left-pad to 32 bytes
            let bytes_slice = string_value.as_bytes();
            if bytes_slice.len() > 32 {
                return Err(invalid_arguments_error("Encoded bytes length exceeds 32 bytes", &bf.span));
            }

            let mut bytes_array = [0u8; 32];
            bytes_array[32 - bytes_slice.len()..].copy_from_slice(bytes_slice);
            let push_value = PushValue::from(bytes_array);

            let push_bytes = push_value.to_hex_with_opcode(evm_version);
            *offset += push_bytes.len() / 2;
            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
        }
        BuiltinFunctionKind::AssertPc => {
            validate_arg_count(bf, 1, "__ASSERT_PC")?;

            // Extract the expected position from the argument
            let expected_position = match &bf.args[0] {
                BuiltinFunctionArg::HexLiteral(hex_str, _) => usize::from_str_radix(hex_str, 16).map_err(|_| {
                    invalid_arguments_error(format!("Invalid hex literal value passed to __ASSERT_PC: 0x{}", hex_str), &bf.span)
                })?,
                BuiltinFunctionArg::Constant(name, span) => {
                    // Evaluate constant directly to numeric value without generating bytecode
                    evaluate_constant_value(name, contract, span)?
                }
                _ => {
                    return Err(invalid_arguments_error("Expected hex literal or constant value as argument to __ASSERT_PC", &bf.span));
                }
            };

            // Create a placeholder for PC assertion validation
            // This will be checked after jump relaxation in fill_unmatched()
            // to ensure the assertion uses post-relaxation offsets
            bytes.push_with_offset(
                starting_offset,
                Bytes::AssertPcPlaceholder(AssertPcPlaceholderData::new(expected_position, bf.span.clone())),
            );

            tracing::debug!(
                target: "codegen",
                "Created __ASSERT_PC placeholder: expecting offset 0x{:x} (will validate after relaxation)",
                expected_position
            );

            // No bytecode is generated - this is a compile-time validation placeholder
        }
    }
    Ok(())
}

/// Generates bytecode for the __codesize builtin function
///
/// Returns a tuple of (byte_offset, Bytes variant) containing the size of the specified macro.
/// Handles circular references by creating a CircularCodesizePlaceholder that's filled in later.
#[allow(clippy::too_many_arguments)]
fn codesize<'a>(
    evm_version: &EVMVersion,
    contract: &'a Contract,
    macro_def: &MacroDefinition,
    scope_mgr: &mut ScopeManager<'a>,
    offset: &mut usize,
    circular_codesize_invocations: &mut CircularCodeSizeIndices,
    bf: &BuiltinFunctionCall,
    relax_jumps: bool,
) -> Result<(usize, Bytes), CodegenError> {
    validate_arg_count(bf, 1, "__codesize")?;

    let macro_name = match &bf.args[0] {
        BuiltinFunctionArg::Identifier(name, _) => name,
        _ => {
            return Err(invalid_arguments_error("Expected identifier as argument to __codesize", &bf.span));
        }
    };

    let ir_macro = if let Some(m) = contract.find_macro_by_name(macro_name) {
        m
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING MACRO PASSED TO __codesize \"{}\"",
            macro_name
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingMacroDefinition(macro_name.to_string()),
            span: bf.span.clone_box(),
            token: None,
        });
    };

    // Get the name of the macro being passed to __codesize
    let codesize_arg = macro_name;
    let is_previous_parent = scope_mgr.macro_stack().iter().any(|def| def.name == *codesize_arg);

    // Special case:
    // If the macro provided to __codesize is the current macro, we need to avoid a
    // circular reference If this is the case we will store a
    // placeholder inside the bytecode and fill it in later when
    // we have adequate information about the macros eventual size.
    // We also need to avoid if the codesize arg is any of the previous macros to
    // avoid a circular reference
    let (codesize_offset, bytes_variant) = if is_previous_parent || macro_def.name.eq(codesize_arg) {
        tracing::debug!(target: "codegen", "CIRCULAR CODESIZE INVOCATION DETECTED INJECTING PLACEHOLDER | macro: {}", ir_macro.name);

        // Save the invocation for later
        circular_codesize_invocations.insert((codesize_arg.to_string(), *offset));

        // Create a CircularCodesizePlaceholder variant (starts as PUSH1, 2 bytes)
        let placeholder = Bytes::CircularCodesizePlaceholder(CircularCodesizePlaceholderData::new(codesize_arg.to_string()));
        (2, placeholder)
    } else {
        // We will still need to recurse to get accurate values
        let res: BytecodeRes = match Codegen::macro_to_bytecode(
            evm_version,
            ir_macro,
            contract,
            scope_mgr,
            *offset,
            ir_macro.name.eq("CONSTRUCTOR"),
            Some(circular_codesize_invocations),
            relax_jumps,
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

        let size = format_even_bytes(format!("{:02x}", res.bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>()));
        let push_bytes = format!("{:02x}{size}", 95 + size.len() / 2);
        let offset = push_bytes.len() / 2;
        (offset, Bytes::Raw(push_bytes))
    };
    Ok((codesize_offset, bytes_variant))
}

/// Generates a PushValue for the __ERROR builtin function
///
/// Returns a PushValue containing the error selector.
/// For error definitions: selector at start, right-padded with zeros (PUSH32 equivalent).
/// For raw signatures: left-padded selector (PUSH4 equivalent).
pub fn error(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<PushValue, CodegenError> {
    validate_arg_count(bf, 1, "__ERROR")?;

    let arg_str = match &bf.args[0] {
        BuiltinFunctionArg::StringLiteral(s, _) => s.clone(),
        BuiltinFunctionArg::Identifier(name, _) => name.clone(),
        _ => {
            return Err(invalid_arguments_error("Expected string literal or identifier as argument to __ERROR", &bf.span));
        }
    };

    let mut bytes = [0u8; 32];
    if let Some(error) = contract.errors.iter().find(|e| e.name.eq(&arg_str)) {
        // Put selector at start, right-padded with zeros (rest is already zero)
        bytes[0..4].copy_from_slice(&error.selector);
    } else {
        let selector: [u8; 4] = keccak256(&arg_str)[..4].try_into().unwrap();
        // Left-pad the selector (put at end)
        bytes[28..32].copy_from_slice(&selector);
    }

    Ok(PushValue::from(bytes))
}

/// Generates bytecode for the __tablesize builtin function
///
/// Returns a tuple of (TableDefinition, bytecode_string) with the size of the specified jump table.
/// The bytecode is a PUSH instruction with the table size in bytes.
pub fn tablesize(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<(TableDefinition, String), CodegenError> {
    validate_arg_count(bf, 1, "__tablesize")?;

    let table_name = match &bf.args[0] {
        BuiltinFunctionArg::Identifier(name, _) => name.clone(),
        _ => {
            return Err(invalid_arguments_error("Expected identifier as argument to __tablesize", &bf.span));
        }
    };

    let ir_table = if let Some(t) = contract.find_table_by_name(&table_name) {
        t
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING TABLE PASSED TO __tablesize \"{}\"",
            table_name
        );
        return Err(CodegenError { kind: CodegenErrorKind::InvalidMacroInvocation(table_name), span: bf.span.clone_box(), token: None });
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
        BuiltinFunctionArg::HexLiteral(hex, _) => hex.clone(),
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
            tracing::error!(target: "codegen", "Invalid argument type passed to {direction}: {:?}", bf.args[0]);
            return Err(invalid_arguments_error(format!("Invalid argument type passed to {direction}"), &bf.span));
        }
    };

    // Ensure hex string has even length for decoding
    // For RIGHTPAD with odd-length hex, append "0" on the right to preserve visual appearance
    // For LEFTPAD with odd-length hex, prepend "0" on the left
    let hex = if first_arg.len() % 2 == 1 {
        if direction == PadDirection::Right { format!("{first_arg}0") } else { format!("0{first_arg}") }
    } else {
        first_arg
    };

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
