use huff_neo_utils::bytecode::BytecodeSegments;
use huff_neo_utils::prelude::*;

use crate::Codegen;
use crate::irgen::builtin_function;

/// Type alias for statement generation result: (bytes, spans)
type StatementGenResult = (BytecodeSegments, Vec<Option<(usize, usize)>>);

/// Generates the respective Bytecode for a given Statement
/// Returns a tuple of (bytes, spans) where spans are optional source positions
#[allow(clippy::too_many_arguments)]
pub fn statement_gen<'a>(
    evm_version: &EVMVersion,
    s: &Statement,
    contract: &'a Contract,
    macro_def: &MacroDefinition,
    scope: &mut Vec<&'a MacroDefinition>,
    offset: &mut usize,
    mis: &mut Vec<(usize, MacroInvocation)>,
    jump_table: &mut JumpTable,
    label_indices: &mut LabelIndices,
    table_instances: &mut Jumps,
    utilized_tables: &mut Vec<TableDefinition>,
    circular_codesize_invocations: &mut CircularCodeSizeIndices,
    starting_offset: usize,
) -> Result<StatementGenResult, CodegenError> {
    let mut bytes = BytecodeSegments::new();
    let mut spans = vec![];

    tracing::debug!(target: "codegen", "Got Statement: {}", s.ty);

    match &s.ty {
        StatementType::MacroInvocation(mi) => {
            // Get the macro definition that matches the name of this invocation
            let ir_macro = if let Some(m) = contract.find_macro_by_name(&mi.macro_name) {
                m
            } else {
                tracing::error!(
                    target: "codegen",
                    "MISSING MACRO INVOCATION \"{}\"",
                    mi.macro_name
                );
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidMacroInvocation(mi.macro_name.clone()),
                    span: mi.span.clone_box(),
                    token: None,
                });
            };

            tracing::info!(target: "codegen", "FOUND INNER MACRO: {}", ir_macro.name);

            if ir_macro.parameters.len() != mi.args.len() {
                tracing::error!(target: "codegen", "MISMATCHED ARGUMENT COUNT FOR {}: {} != {}", ir_macro.name, ir_macro.parameters.len(), mi.args.len());
                return Err(CodegenError {
                    kind: CodegenErrorKind::InvalidArgumentCount(ir_macro.name.clone(), ir_macro.parameters.len(), mi.args.len()),
                    span: mi.span.clone_box(),
                    token: None,
                });
            }

            // Tests may not be invoked
            if ir_macro.test {
                tracing::error!(target: "codegen", "Tests may not be invoked: {}", ir_macro.name);
                return Err(CodegenError {
                    kind: CodegenErrorKind::TestInvocation(ir_macro.name.clone()),
                    span: ir_macro.span.clone_box(),
                    token: None,
                });
            }

            // If invoked macro is a function (outlined), insert a jump to the function's code and a
            // jumpdest to return to. If it is inlined, insert the macro's code at the
            // current offset.
            if ir_macro.outlined {
                // Get necessary swap ops to reorder stack
                // PC of the return jumpdest should be below the function's stack inputs
                let stack_swaps = (0..ir_macro.takes).rev().map(|i| format!("{:02x}", 0x90 + i)).collect::<Vec<_>>();

                // Insert a jump to the outlined macro's code
                // Use offset to make each invocation unique
                let scope_path: Vec<String> = if scope.len() > 1 {
                    let mut path: Vec<String> = scope[..scope.len() - 1].iter().map(|m| m.name.clone()).collect();
                    path.push(format!("{}_{}", scope.last().unwrap().name, *offset));
                    path
                } else {
                    scope.iter().map(|m| m.name.clone()).collect()
                };
                let scope_depth = scope.len().saturating_sub(1);
                jump_table.insert(
                    *offset + stack_swaps.len() + 3, // PUSH2 + 2 bytes + stack_swaps.len()
                    vec![Jump {
                        label: format!("goto_{}", &ir_macro.name),
                        bytecode_index: 0,
                        span: s.span.clone(),
                        scope_depth,
                        scope_path,
                    }],
                );

                // Get span info for this statement
                // Convert from original file coordinates to flattened coordinates
                let span_info = if !s.span.0.is_empty() {
                    s.span.0.first().and_then(|sp| {
                        if sp.start != 0 || sp.end != 0 {
                            // Convert original span to flattened coordinates
                            contract.map_original_span_to_flattened(sp)
                        } else {
                            None
                        }
                    })
                } else {
                    None
                };

                // Store return JUMPDEST PC on the stack and re-order the stack so that
                // the return JUMPDEST PC is below the function's stack inputs
                bytes.push_with_offset(
                    *offset,
                    Bytes::Raw(format!("{}{:04x}{}", Opcode::Push2, *offset + stack_swaps.len() + 7, stack_swaps.join(""))),
                );
                spans.push(span_info);
                // Insert jump to outlined macro + jumpdest to return to
                bytes.push_with_offset(
                    *offset + stack_swaps.len() + 3, // PUSH2 + 2 bytes + stack_swaps.len()
                    Bytes::JumpPlaceholder(format!("{}xxxx{}{}", Opcode::Push2, Opcode::Jump, Opcode::Jumpdest)),
                );
                spans.push(span_info);
                // PUSH2 + 2 bytes + stack_swaps.len() + PUSH2 + 2 bytes + JUMP + JUMPDEST
                *offset += stack_swaps.len() + 8;
            } else {
                // Check for circular recursion - if this macro is already in the scope, it's recursive
                if scope.iter().any(|m| m.name == ir_macro.name) {
                    tracing::error!(
                        target: "codegen",
                        "CIRCULAR RECURSION DETECTED: Macro \"{}\" is already in the call stack",
                        ir_macro.name
                    );
                    return Err(CodegenError {
                        kind: CodegenErrorKind::CircularMacroInvocation(ir_macro.name.clone()),
                        span: mi.span.clone_box(),
                        token: None,
                    });
                }

                // Check recursion depth limit to prevent stack overflow
                const MAX_RECURSION_DEPTH: usize = 100;
                if scope.len() >= MAX_RECURSION_DEPTH {
                    tracing::error!(
                        target: "codegen",
                        "RECURSION DEPTH LIMIT EXCEEDED: Maximum depth of {} reached",
                        MAX_RECURSION_DEPTH
                    );
                    return Err(CodegenError {
                        kind: CodegenErrorKind::RecursionDepthExceeded(MAX_RECURSION_DEPTH),
                        span: mi.span.clone_box(),
                        token: None,
                    });
                }

                // Recurse into macro invocation
                scope.push(ir_macro);

                // Resolve any ArgCall and ArgCallMacroInvocation arguments before pushing the invocation
                let mut resolved_args = Vec::new();
                for arg in &mi.args {
                    match arg {
                        MacroArg::ArgCall(arg_call) => {
                            // Resolve this ArgCall by looking it up in the current context
                            let mut resolved = None;

                            // Search through the invocation stack for the argument's value
                            for (_, parent_mi) in mis.iter().rev() {
                                if let Some(parent_macro) = contract.find_macro_by_name(&parent_mi.macro_name)
                                    && let Some(param_idx) =
                                        parent_macro.parameters.iter().position(|p| p.name.as_deref() == Some(&arg_call.name))
                                    && let Some(arg_value) = parent_mi.args.get(param_idx)
                                {
                                    resolved = Some(arg_value.clone());
                                    break;
                                }
                            }

                            // Use resolved value or keep the ArgCall if not found
                            resolved_args.push(resolved.unwrap_or_else(|| arg.clone()));
                        }
                        MacroArg::ArgCallMacroInvocation(arg_name, invoc_args) => {
                            // Resolve the argument name to find the actual macro
                            let mut resolved_macro_name = None;

                            // Search through the invocation stack for the argument's value
                            for (_, parent_mi) in mis.iter().rev() {
                                if let Some(parent_macro) = contract.find_macro_by_name(&parent_mi.macro_name)
                                    && let Some(param_idx) =
                                        parent_macro.parameters.iter().position(|p| p.name.as_deref() == Some(arg_name))
                                    && let Some(MacroArg::Ident(macro_name)) = parent_mi.args.get(param_idx)
                                {
                                    resolved_macro_name = Some(macro_name.clone());
                                    break;
                                }
                            }

                            // If we resolved the macro name, create a regular MacroCall
                            if let Some(macro_name) = resolved_macro_name {
                                resolved_args.push(MacroArg::MacroCall(MacroInvocation {
                                    macro_name,
                                    args: invoc_args.clone(),
                                    span: AstSpan(vec![]),
                                }));
                            } else {
                                // Keep the original if we couldn't resolve
                                resolved_args.push(arg.clone());
                            }
                        }
                        _ => resolved_args.push(arg.clone()),
                    }
                }

                // Create a resolved invocation
                let resolved_mi = MacroInvocation { macro_name: mi.macro_name.clone(), args: resolved_args, span: mi.span.clone() };

                mis.push((*offset, resolved_mi));

                let mut res: BytecodeRes = match Codegen::macro_to_bytecode(
                    evm_version,
                    ir_macro,
                    contract,
                    scope,
                    *offset,
                    mis,
                    false,
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

                // Set jump table values
                tracing::debug!(target: "codegen", "Unmatched jumps: {:?}", res.unmatched_jumps.iter().map(|uj| uj.label.clone()).collect::<Vec<String>>());
                for j in res.unmatched_jumps.iter_mut() {
                    let new_index = j.bytecode_index;
                    j.bytecode_index = 0;
                    let mut new_jumps = if let Some(jumps) = jump_table.get(&new_index) { jumps.clone() } else { vec![] };
                    new_jumps.push(j.clone());
                    jump_table.insert(new_index, new_jumps);
                }
                tracing::debug!(target: "codegen", "Extending table_instances with {} items from macro {}", res.table_instances.len(), ir_macro.name);
                table_instances.extend(res.table_instances);
                label_indices.extend(res.label_indices);
                // Only add tables that are not already in the utilized tables
                let res_unique_tables =
                    res.utilized_tables.iter().filter(|t| !utilized_tables.contains(t)).cloned().collect::<Vec<TableDefinition>>();
                utilized_tables.extend(res_unique_tables);

                // Increase offset by byte length of recursed macro
                *offset += res.bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>() / 2;
                // Add the macro's bytecode and spans to the final result
                bytes.extend(res.bytes);
                // Preserve the spans from the nested macro expansion
                spans.extend(res.spans)
            }
        }
        StatementType::Label(label) => {
            // Add JUMPDEST opcode to final result and add to label_indices
            tracing::info!(target: "codegen", "RECURSE BYTECODE GOT LABEL: {:?}", label.name);

            // Build the scope path from the current macro invocation stack
            // Use offset to make each invocation unique
            let scope_path: Vec<String> = if scope.len() > 1 {
                let mut path: Vec<String> = scope[..scope.len() - 1].iter().map(|m| m.name.clone()).collect();
                // Add the current macro with its invocation offset to make it unique
                path.push(format!("{}_{}", scope.last().unwrap().name, *offset));
                path
            } else {
                scope.iter().map(|m| m.name.clone()).collect()
            };
            let scope_depth = scope.len().saturating_sub(1); // -1 because scope includes the current macro

            // Try to insert the label with scope information
            if let Err(err_msg) = label_indices.insert(label.name.clone(), *offset, scope_depth, scope_path) {
                tracing::error!(target: "codegen", "DUPLICATE LABEL: {}", err_msg);
                return Err(CodegenError {
                    kind: CodegenErrorKind::DuplicateLabelInScope(label.name.clone()),
                    span: s.span.clone_box(),
                    token: None,
                });
            }

            bytes.push_with_offset(*offset, Bytes::Raw(Opcode::Jumpdest.to_string()));
            // Add span for the JUMPDEST
            let span_info = if !s.span.0.is_empty() {
                s.span.0.first().and_then(|sp| {
                    if sp.start != 0 || sp.end != 0 {
                        // Convert original span to flattened coordinates
                        contract.map_original_span_to_flattened(sp)
                    } else {
                        None
                    }
                })
            } else {
                None
            };
            spans.push(span_info);
            *offset += 1;
        }
        StatementType::LabelCall(label) => {
            // Generate code for a `LabelCall`
            // PUSH2 + 2 byte destination (placeholder for now, filled in `Codegen::fill_unmatched`
            tracing::info!(target: "codegen", "RECURSE BYTECODE GOT LABEL CALL: {}", label);

            // Build the scope information for this jump
            // Use offset to make each invocation unique (matching the label definition logic)
            let scope_path: Vec<String> = if scope.len() > 1 {
                let mut path: Vec<String> = scope[..scope.len() - 1].iter().map(|m| m.name.clone()).collect();
                // Add the current macro with its invocation offset to make it unique
                path.push(format!("{}_{}", scope.last().unwrap().name, *offset));
                path
            } else {
                scope.iter().map(|m| m.name.clone()).collect()
            };
            let scope_depth = scope.len().saturating_sub(1); // -1 because scope includes the current macro

            jump_table
                .insert(*offset, vec![Jump { label: label.to_string(), bytecode_index: 0, span: s.span.clone(), scope_depth, scope_path }]);
            bytes.push_with_offset(*offset, Bytes::JumpPlaceholder(format!("{}xxxx", Opcode::Push2)));
            // Add span for the PUSH2
            let span_info = if !s.span.0.is_empty() {
                s.span.0.first().and_then(|sp| {
                    if sp.start != 0 || sp.end != 0 {
                        // Convert original span to flattened coordinates
                        contract.map_original_span_to_flattened(sp)
                    } else {
                        None
                    }
                })
            } else {
                None
            };
            spans.push(span_info);
            *offset += 3;
        }
        StatementType::BuiltinFunctionCall(bf) => {
            let bytes_before = bytes.len();
            builtin_function::builtin_function_gen(
                evm_version,
                contract,
                macro_def,
                scope,
                offset,
                mis,
                table_instances,
                utilized_tables,
                circular_codesize_invocations,
                starting_offset,
                &mut bytes,
                bf,
            )?;
            // Add spans for the bytes added by builtin function
            let bytes_added = bytes.len() - bytes_before;
            let span_info = if !s.span.0.is_empty() {
                s.span.0.first().and_then(|sp| {
                    if sp.start != 0 || sp.end != 0 {
                        // Convert original span to flattened coordinates
                        contract.map_original_span_to_flattened(sp)
                    } else {
                        None
                    }
                })
            } else {
                None
            };
            for _ in 0..bytes_added {
                spans.push(span_info);
            }
        }
        StatementType::ArgMacroInvocation(_parent_macro_name, arg_name, args) => {
            // Handle macro invocation through argument
            // We need to resolve the argument to get the actual macro name
            tracing::info!(target: "codegen", "Processing ArgMacroInvocation: <{}>()", arg_name);

            // Function to recursively resolve an argument through the invocation stack
            fn resolve_argument(arg_name: &str, mis: &[(usize, MacroInvocation)], contract: &Contract) -> Result<String, CodegenErrorKind> {
                // Search through the macro invocation stack
                for (_, mi) in mis.iter().rev() {
                    // Find the macro definition
                    if let Some(invoked_macro) = contract.find_macro_by_name(&mi.macro_name) {
                        // Check if this macro has our argument
                        if let Some(param_index) = invoked_macro.parameters.iter().position(|p| p.name.as_deref() == Some(arg_name)) {
                            // Get the corresponding argument value
                            if let Some(arg_value) = mi.args.get(param_index) {
                                match arg_value {
                                    MacroArg::Ident(macro_name) => {
                                        // Direct macro name - we're done
                                        return Ok(macro_name.clone());
                                    }
                                    MacroArg::ArgCall(arg_call) => {
                                        // The argument is itself an argument reference - need to resolve it recursively
                                        return resolve_argument(&arg_call.name, mis, contract);
                                    }
                                    MacroArg::ArgCallMacroInvocation(inner_arg_name, _) => {
                                        // The argument is an invocation of another argument - resolve that argument
                                        return resolve_argument(inner_arg_name, mis, contract);
                                    }
                                    MacroArg::Noop => {
                                        // __NOOP cannot be invoked as a macro
                                        return Err(CodegenErrorKind::InvalidMacroArgumentType(format!(
                                            "Cannot invoke __NOOP as a macro in argument '{}'",
                                            arg_name
                                        )));
                                    }
                                    _ => {
                                        // Other types of arguments not supported for macro invocation
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
                Err(CodegenErrorKind::MissingArgumentDefinition(arg_name.to_string()))
            }

            // Resolve the argument to get the actual macro name
            let macro_name = match resolve_argument(arg_name, mis, contract) {
                Ok(name) => {
                    tracing::info!(target: "codegen", "Resolved <{}> to macro: {}", arg_name, name);
                    name
                }
                Err(error_kind) => {
                    tracing::error!(target: "codegen", "Failed to resolve argument '{}': {:?}", arg_name, error_kind);
                    return Err(CodegenError { kind: error_kind, span: s.span.clone_box(), token: None });
                }
            };

            // Create a MacroInvocation with the resolved macro name
            let resolved_invocation = MacroInvocation { macro_name, args: args.clone(), span: s.span.clone() };

            // Process it as a regular macro invocation
            let macro_invocation_statement =
                Statement { ty: StatementType::MacroInvocation(resolved_invocation.clone()), span: s.span.clone() };

            // Recursively process the resolved macro invocation
            return statement_gen(
                evm_version,
                &macro_invocation_statement,
                contract,
                macro_def,
                scope,
                offset,
                mis,
                jump_table,
                label_indices,
                table_instances,
                utilized_tables,
                circular_codesize_invocations,
                starting_offset,
            );
        }
        sty => {
            tracing::error!(target: "codegen", "CURRENT MACRO DEF: {}", macro_def.name);
            tracing::error!(target: "codegen", "UNEXPECTED STATEMENT: {:?}", sty);
            return Err(CodegenError { kind: CodegenErrorKind::InvalidMacroStatement, span: s.span.clone_box(), token: None });
        }
    }

    Ok((bytes, spans))
}
