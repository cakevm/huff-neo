use crate::Codegen;
use crate::irgen::constants::constant_gen;
use alloy_primitives::{hex, keccak256};
use huff_neo_utils::bytecode::{BytecodeRes, Bytes, CircularCodeSizeIndices, Jump, Jumps};
use huff_neo_utils::bytes_util::{bytes32_to_hex_string, format_even_bytes, literal_gen, pad_n_bytes};
use huff_neo_utils::error::{CodegenError, CodegenErrorKind};
use huff_neo_utils::evm_version::EVMVersion;
use huff_neo_utils::opcodes::Opcode;
use huff_neo_utils::prelude::{
    BuiltinFunctionArg, BuiltinFunctionCall, BuiltinFunctionKind, Contract, MacroDefinition, MacroInvocation, TableDefinition,
};
use std::fmt::Display;

// TODO: First step to refactor and split the function into smaller functions
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
    bytes: &mut Vec<(usize, Bytes)>,
    bf: &BuiltinFunctionCall,
) -> Result<(), CodegenError> {
    // Generate code for a `BuiltinFunctionCall`
    tracing::info!(target: "codegen", "RECURSE BYTECODE GOT BUILTIN FUNCTION CALL: {:?}", bf);
    match bf.kind {
        BuiltinFunctionKind::Codesize => {
            let (codesize_offset, push_bytes) =
                codesize(evm_version, contract, macro_def, scope, offset, mis, circular_codesize_invocations, bf)?;

            *offset += codesize_offset;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::Tablesize => {
            let (ir_table, push_bytes) = tablesize(contract, bf)?;

            if !utilized_tables.contains(&ir_table) {
                utilized_tables.push(ir_table);
            }

            *offset += push_bytes.len() / 2;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::Tablestart => {
            let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __tablestart")),
                    span: bf.span.clone(),
                    token: None,
                });
            };
            // Make sure the table exists
            if let Some(t) = contract.find_table_by_name(first_arg.name.as_ref().unwrap()) {
                table_instances.push(Jump {
                    label: first_arg.name.as_ref().unwrap().to_owned(),
                    bytecode_index: *offset,
                    span: bf.span.clone(),
                });
                if !utilized_tables.contains(&t) {
                    utilized_tables.push(t);
                }

                bytes.push((*offset, Bytes(format!("{}xxxx", Opcode::Push2))));
                *offset += 3;
            } else {
                tracing::error!(
                    target: "codegen",
                    "MISSING TABLE PASSED TO __tablestart \"{}\"",
                    first_arg.name.as_ref().unwrap()
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidMacroInvocation(first_arg.name.as_ref().unwrap().to_string()),
                    span: bf.span.clone(),
                    token: None,
                });
            }
        }
        BuiltinFunctionKind::FunctionSignature => {
            let push_bytes = function_signature(contract, bf)?;

            *offset += push_bytes.len() / 2;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::EventHash => {
            let push_bytes = event_hash(contract, bf)?;

            *offset += push_bytes.len() / 2;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::Error => {
            let push_bytes = error(contract, bf)?;

            *offset += push_bytes.len() / 2;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::RightPad => {
            let push_bytes = builtin_pad(evm_version, contract, bf, PadDirection::Right)?;
            *offset += push_bytes.len() / 2;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::LeftPad => {
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidArguments(String::from("LeftPad is not supported in a function or macro")),
                span: bf.span.clone(),
                token: None,
            });
        }
        BuiltinFunctionKind::DynConstructorArg => {
            if bf.args.len() != 2 {
                tracing::error!(
                    target = "codegen",
                    "Incorrect number of arguments passed to __CODECOPY_DYN_ARG, should be 2: {}",
                    bf.args.len()
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(format!(
                        "Incorrect number of arguments passed to __CODECOPY_DYN_ARG, should be 2: {}",
                        bf.args.len()
                    )),
                    span: bf.span.clone(),
                    token: None,
                });
            }

            let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __CODECOPY_DYN_ARG")),
                    span: bf.span.clone(),
                    token: None,
                });
            };
            let BuiltinFunctionArg::Argument(ref second_arg) = bf.args[1] else {
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __CODECOPY_DYN_ARG")),
                    span: bf.span.clone(),
                    token: None,
                });
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
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(String::from(
                        "Incorrect number of bytes in argument passed to __CODECOPY_DYN_ARG. Should be (1 byte, <= 2 bytes)",
                    )),
                    span: bf.span.clone(),
                    token: None,
                });
            }

            // Insert a 17 byte placeholder- will be filled when constructor args are added
            // to the end of the runtime code.
            // <len (2 bytes)> <dest_mem_ptr (2 bytes)> mstore
            // <len (2 bytes)> <contents_code_ptr (2 bytes)> <dest_mem_ptr + 0x20 (2 bytes)>
            // codecopy
            *offset += 17;
            bytes.push((
                starting_offset,
                Bytes(format!(
                    "{}{}{}",
                    "xx".repeat(14),
                    first_arg.name.as_ref().unwrap(),
                    pad_n_bytes(second_arg.name.as_ref().unwrap(), 2)
                )),
            ));
        }
        BuiltinFunctionKind::Verbatim => {
            if bf.args.len() != 1 {
                tracing::error!(target = "codegen", "Incorrect number of arguments passed to __VERBATIM, should be 1: {}", bf.args.len());
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(format!(
                        "Incorrect number of arguments passed to __VERBATIM, should be 1: {}",
                        bf.args.len()
                    )),
                    span: bf.span.clone(),
                    token: None,
                });
            }

            let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __VERBATIM")),
                    span: bf.span.clone(),
                    token: None,
                });
            };
            let verbatim_str = first_arg.name.as_ref().unwrap();
            // check if verbatim was passed a hex string
            let mut is_hex = true;
            for c in verbatim_str.chars() {
                if !c.is_ascii_hexdigit() {
                    is_hex = false;
                    break;
                }
            }
            if !is_hex {
                tracing::error!(
                    target: "codegen",
                    "INVALID HEX STRING PASSED TO __VERBATIM: \"{}\"",
                    first_arg.name.as_ref().unwrap()
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidHex(verbatim_str.to_string()),
                    span: bf.span.clone(),
                    token: None,
                });
            }

            tracing::debug!(target: "codegen", "INJECTING as verbatim: {}", verbatim_str);
            let hex = format_even_bytes(verbatim_str.clone());
            let push_bytes = hex.to_string();
            *offset += hex.len() / 2;

            bytes.push((starting_offset, Bytes(push_bytes)));
        }
        BuiltinFunctionKind::Bytes => {
            let push_bytes = builtin_bytes(evm_version, bf)?;
            *offset += push_bytes.len() / 2;
            bytes.push((starting_offset, Bytes(push_bytes)));
        }
    }
    Ok(())
}

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
    let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __codesize")),
            span: bf.span.clone(),
            token: None,
        });
    };
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
            span: bf.span.clone(),
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

        let size = format_even_bytes(format!("{:02x}", (res.bytes.iter().map(|(_, b)| b.0.len()).sum::<usize>() / 2)));
        let push_bytes = format!("{:02x}{size}", 95 + size.len() / 2);
        let offset = push_bytes.len() / 2;
        (offset, push_bytes)
    };
    Ok((codesize_offset, push_bytes))
}

pub fn error(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<String, CodegenError> {
    if bf.args.len() != 1 {
        tracing::error!(
            target: "codegen",
            "Incorrect number of arguments passed to __ERROR, should be 1: {}",
            bf.args.len()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(format!(
                "Incorrect number of arguments passed to __ERROR, should be 1: {}",
                bf.args.len()
            )),
            span: bf.span.clone(),
            token: None,
        });
    }
    let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __ERROR")),
            span: bf.span.clone(),
            token: None,
        });
    };
    let push_bytes = if let Some(error) = contract.errors.iter().find(|e| first_arg.name.as_ref().unwrap().eq(&e.name)) {
        // Add 28 bytes to left-pad the 4 byte selector
        let selector = format!("{}{}", hex::encode(error.selector), "00".repeat(28));
        format!("{}{selector}", Opcode::Push32)
    } else if let Some(s) = &first_arg.name {
        let function_selector: [u8; 4] = keccak256(s)[..4].try_into().unwrap();
        format!("{}{}", Opcode::Push4, hex::encode(function_selector))
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING ERROR DEFINITION PASSED TO __ERROR: \"{}\"",
            first_arg.name.as_ref().unwrap()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingErrorDefinition(first_arg.name.as_ref().unwrap().to_string()),
            span: bf.span.clone(),
            token: None,
        });
    };
    Ok(push_bytes)
}

pub fn tablesize(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<(TableDefinition, String), CodegenError> {
    let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect arguments type passed to __tablesize")),
            span: bf.span.clone(),
            token: None,
        });
    };
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
            span: bf.span.clone(),
            token: None,
        });
    };
    let Some(table_size) = ir_table.size else {
        return Err(CodegenError { kind: CodegenErrorKind::MissingTableSize(ir_table.name.clone()), span: bf.span.clone(), token: None });
    };
    let size = bytes32_to_hex_string(&table_size, false);
    let push_bytes = format!("{:02x}{size}", 95 + size.len() / 2);
    Ok((ir_table, push_bytes))
}

pub fn event_hash(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<String, CodegenError> {
    if bf.args.len() != 1 {
        tracing::error!(
            target: "codegen",
            "Incorrect number of arguments passed to __EVENT_HASH, should be 1: {}",
            bf.args.len()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(format!(
                "Incorrect number of arguments passed to __EVENT_HASH, should be 1: {}",
                bf.args.len()
            )),
            span: bf.span.clone(),
            token: None,
        });
    }
    let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect argument type passed to __EVENT_HASH")),
            span: bf.span.clone(),
            token: None,
        });
    };
    let push_bytes = if let Some(event) = contract.events.iter().find(|e| first_arg.name.as_ref().unwrap().eq(&e.name)) {
        let hash = bytes32_to_hex_string(&event.hash, false);
        format!("{}{hash}", Opcode::Push32)
    } else if let Some(s) = &first_arg.name {
        let event_selector = keccak256(s).0;
        format!("{}{}", Opcode::Push32, hex::encode(event_selector))
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING EVENT INTERFACE PASSED TO __EVENT_HASH: \"{}\"",
            first_arg.name.as_ref().unwrap()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingEventInterface(first_arg.name.as_ref().unwrap().to_string()),
            span: bf.span.clone(),
            token: None,
        });
    };
    Ok(push_bytes)
}

pub fn function_signature(contract: &Contract, bf: &BuiltinFunctionCall) -> Result<String, CodegenError> {
    if bf.args.len() != 1 {
        tracing::error!(
            target: "codegen",
            "Incorrect number of arguments passed to __FUNC_SIG, should be 1: {}",
            bf.args.len()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(format!(
                "Incorrect number of arguments passed to __FUNC_SIG, should be 1: {}",
                bf.args.len()
            )),
            span: bf.span.clone(),
            token: None,
        });
    }
    let BuiltinFunctionArg::Argument(ref first_arg) = bf.args[0] else {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Incorrect argument type passed to __FUNC_SIG")),
            span: bf.span.clone(),
            token: None,
        });
    };
    let push_bytes = if let Some(func) = contract.functions.iter().find(|f| first_arg.name.as_ref().unwrap().eq(&f.name)) {
        format!("{}{}", Opcode::Push4, hex::encode(func.signature))
    } else if let Some(error) = contract.errors.iter().find(|e| first_arg.name.as_ref().unwrap().eq(&e.name)) {
        format!("{}{}", Opcode::Push4, hex::encode(error.selector))
    } else if let Some(s) = &first_arg.name {
        let function_selector: [u8; 4] = keccak256(s)[..4].try_into().unwrap();
        format!("{}{}", Opcode::Push4, hex::encode(function_selector))
    } else {
        tracing::error!(
            target: "codegen",
            "MISSING FUNCTION INTERFACE PASSED TO __SIG: \"{}\"",
            first_arg.name.as_ref().unwrap()
        );
        return Err(CodegenError {
            kind: CodegenErrorKind::MissingFunctionInterface(first_arg.name.as_ref().unwrap().to_string()),
            span: bf.span.clone(),
            token: None,
        });
    };

    Ok(push_bytes)
}

#[derive(Debug, Clone, PartialEq)]
pub enum PadDirection {
    Left,
    Right,
}
impl Display for PadDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PadDirection::Left => write!(f, "__LEFTPAD"),
            PadDirection::Right => write!(f, "__RIGHTPAD"),
        }
    }
}

pub fn builtin_pad(
    evm_version: &EVMVersion,
    contract: &Contract,
    bf: &BuiltinFunctionCall,
    direction: PadDirection,
) -> Result<String, CodegenError> {
    if bf.args.len() != 1 {
        tracing::error!(target = "codegen", "Incorrect number of arguments passed to {direction}, should be 1: {}", bf.args.len());
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(format!(
                "Incorrect number of arguments passed to {direction}, should be 1: {}",
                bf.args.len()
            )),
            span: bf.span.clone(),
            token: None,
        });
    }
    let first_arg = match &bf.args[0] {
        BuiltinFunctionArg::Argument(arg) => arg.name.clone().unwrap_or_default(),
        BuiltinFunctionArg::BuiltinFunctionCall(inner_call) => {
            match inner_call.kind {
                BuiltinFunctionKind::FunctionSignature => {
                    let push_bytes = function_signature(contract, inner_call)?;
                    push_bytes[2..].to_string() // remove opcode
                }
                BuiltinFunctionKind::Bytes => {
                    let push_bytes = builtin_bytes(evm_version, inner_call)?;
                    push_bytes[2..].to_string() // remove opcode
                }
                _ => {
                    tracing::error!(target: "codegen", "Invalid function call argument type passed to {direction}");
                    return Err(CodegenError {
                        kind: CodegenErrorKind::InvalidArguments(format!("Invalid argument type passed to {direction}")),
                        span: bf.span.clone(),
                        token: None,
                    });
                }
            }
        }
        BuiltinFunctionArg::Constant(name, span) => {
            let push_bytes = constant_gen(evm_version, name, contract, span)?;
            push_bytes[2..].to_string() // remove opcode
        }
        _ => {
            tracing::error!(target: "codegen", "Invalid argument type passed to {direction}");
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidArguments(format!("Invalid argument type passed to {direction}")),
                span: bf.span.clone(),
                token: None,
            });
        }
    };
    let hex = format_even_bytes(first_arg);
    if direction == PadDirection::Left {
        return Ok(format!("{}{}{hex}", Opcode::Push32, "0".repeat(64 - hex.len())));
    }
    Ok(format!("{}{hex}{}", Opcode::Push32, "0".repeat(64 - hex.len())))
}

pub fn builtin_bytes(evm_version: &EVMVersion, bf: &BuiltinFunctionCall) -> Result<String, CodegenError> {
    if bf.args.len() != 1 {
        tracing::error!(target = "codegen", "Incorrect number of arguments passed to __BYTES, should be 1: {}", bf.args.len());
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(format!(
                "Incorrect number of arguments passed to __BYTES, should be 1: {}",
                bf.args.len()
            )),
            span: bf.span.clone(),
            token: None,
        });
    }
    let first_arg = match bf.args[0] {
        BuiltinFunctionArg::Argument(ref arg) => arg.name.clone().unwrap_or_default(),
        _ => {
            tracing::error!(target: "codegen", "Invalid argument type passed to __BYTES");
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidArguments(String::from("Invalid argument type passed to __BYTES")),
                span: bf.span.clone(),
                token: None,
            });
        }
    };

    if first_arg.is_empty() {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Empty string passed to __BYTES")),
            span: bf.span.clone(),
            token: None,
        });
    }

    let bytes = first_arg.as_bytes();
    if bytes.len() > 32 {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(String::from("Encoded bytes length exceeds 32 bytes")),
            span: bf.span.clone(),
            token: None,
        });
    }
    let mut bytes_array = [0u8; 32];
    bytes_array[32 - bytes.len()..].copy_from_slice(bytes);

    let push_bytes = literal_gen(evm_version, &bytes_array);
    Ok(push_bytes)
}
