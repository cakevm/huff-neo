use crate::Codegen;
use crate::irgen::constants::{constant_gen, evaluate_constant_value};
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
    Argument, AstSpan, BuiltinFunctionArg, BuiltinFunctionCall, BuiltinFunctionKind, Contract, MacroDefinition, PushValue, TableDefinition,
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
            let first_arg = extract_single_argument(bf, "__tablestart")?;
            let table_name = first_arg.name.as_ref().unwrap();

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
            let first_arg = extract_single_argument(bf, "__EMBED_TABLE")?;
            let table_name = first_arg.name.as_ref().unwrap();

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
                Bytes::DynConstructorArgPlaceholder(DynConstructorArgPlaceholderData::new(
                    first_arg.name.as_ref().unwrap().to_string(),
                    pad_n_bytes(second_arg.name.as_ref().unwrap(), 2),
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
