use alloy_primitives::U256;
use huff_neo_utils::bytecode::{BytecodeSegments, JumpPlaceholderData, PushOpcode};
use huff_neo_utils::bytes_util::str_to_bytes32;
use huff_neo_utils::prelude::*;

use crate::Codegen;
use crate::irgen::arg_calls::bubble_arg_call;
use crate::irgen::builtin_function;
use crate::irgen::constants::{constant_gen, lookup_constant};

/// Type alias for statement generation result: (bytes, spans)
type StatementGenResult = (BytecodeSegments, Vec<Option<(usize, usize)>>);

/// Evaluates an expression with access to macro invocation context for resolving `<arg>` calls
fn evaluate_expression_with_context(
    expr: &Expression,
    contract: &Contract,
    mis: &[(usize, MacroInvocation)],
) -> Result<[u8; 32], CodegenError> {
    match expr {
        Expression::ArgCall { name, span: arg_span, .. } => {
            // Search through the macro invocation stack for the argument's value
            for (_, parent_mi) in mis.iter().rev() {
                if let Some(parent_macro) = contract.find_macro_by_name(&parent_mi.macro_name)
                    && let Some(param_idx) = parent_macro.parameters.iter().position(|p| p.name.as_deref() == Some(name))
                    && let Some(arg_value) = parent_mi.args.get(param_idx)
                {
                    // Found the argument - now evaluate it
                    match arg_value {
                        MacroArg::Literal(lit) => {
                            // Direct literal value
                            return Ok(*lit);
                        }
                        MacroArg::Ident(ident_name) => {
                            // Identifier - could be a constant reference, try to look it up and evaluate
                            if let Ok(const_def) = lookup_constant(ident_name, contract, arg_span) {
                                match &const_def.value {
                                    ConstVal::Bytes(bytes) => {
                                        use huff_neo_utils::bytes_util::str_to_bytes32;
                                        return Ok(str_to_bytes32(&bytes.as_str()));
                                    }
                                    ConstVal::Expression(const_expr) => {
                                        return contract.evaluate_constant_expression(const_expr);
                                    }
                                    _ => {
                                        return Err(CodegenError {
                                            kind: CodegenErrorKind::InvalidConstantExpression,
                                            span: arg_span.clone_box(),
                                            token: None,
                                        });
                                    }
                                }
                            } else {
                                // Not a constant - this is an error in expression context
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::InvalidMacroArgumentType(format!(
                                        "Argument '{}' resolved to identifier '{}' which cannot be used in expression",
                                        name, ident_name
                                    )),
                                    span: arg_span.clone_box(),
                                    token: None,
                                });
                            }
                        }
                        MacroArg::BuiltinFunctionCall(bf) => {
                            // Builtin function call - evaluate it
                            let push_value = Codegen::gen_builtin_bytecode(contract, bf, bf.span.clone())?;
                            return Ok(push_value.value.0);
                        }
                        MacroArg::ArgCall(nested_arg) => {
                            // Nested argument reference - recursively resolve
                            return evaluate_expression_with_context(
                                &Expression::ArgCall {
                                    macro_name: String::new(),
                                    name: nested_arg.name.clone(),
                                    span: nested_arg.span.clone(),
                                },
                                contract,
                                mis,
                            );
                        }
                        _ => {
                            return Err(CodegenError {
                                kind: CodegenErrorKind::InvalidMacroArgumentType(format!(
                                    "Argument '{}' cannot be used in expression (unsupported type)",
                                    name
                                )),
                                span: arg_span.clone_box(),
                                token: None,
                            });
                        }
                    }
                }
            }

            // Argument not found in invocation stack
            Err(CodegenError { kind: CodegenErrorKind::MissingArgumentDefinition(name.clone()), span: arg_span.clone_box(), token: None })
        }

        Expression::Binary { left, op, right, span } => {
            // Evaluate both operands using this function to handle nested ArgCalls
            let left_val = evaluate_expression_with_context(left, contract, mis)?;
            let right_val = evaluate_expression_with_context(right, contract, mis)?;

            // Convert to U256 for arithmetic
            let l = U256::from_be_bytes(left_val);
            let r = U256::from_be_bytes(right_val);

            // Perform the operation
            let result = match op {
                BinaryOp::Add => l.wrapping_add(r),
                BinaryOp::Sub => l.wrapping_sub(r),
                BinaryOp::Mul => l.wrapping_mul(r),
                BinaryOp::Div => {
                    if r.is_zero() {
                        return Err(CodegenError { kind: CodegenErrorKind::DivisionByZero, span: span.clone_box(), token: None });
                    }
                    l / r
                }
                BinaryOp::Mod => {
                    if r.is_zero() {
                        return Err(CodegenError { kind: CodegenErrorKind::DivisionByZero, span: span.clone_box(), token: None });
                    }
                    l % r
                }
                BinaryOp::Eq => {
                    if l == r {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
                BinaryOp::NotEq => {
                    if l != r {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
                BinaryOp::Lt => {
                    if l < r {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
                BinaryOp::Gt => {
                    if l > r {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
                BinaryOp::LtEq => {
                    if l <= r {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
                BinaryOp::GtEq => {
                    if l >= r {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
            };

            Ok(result.to_be_bytes())
        }

        Expression::Unary { op, expr: inner_expr, .. } => {
            // Evaluate the operand using this function to handle nested ArgCalls
            let val = evaluate_expression_with_context(inner_expr, contract, mis)?;
            let v = U256::from_be_bytes(val);

            // Perform unary operation
            let result = match op {
                UnaryOp::Neg => U256::ZERO.wrapping_sub(v),
                UnaryOp::Not => {
                    if v.is_zero() {
                        U256::from(1u64)
                    } else {
                        U256::ZERO
                    }
                }
            };

            Ok(result.to_be_bytes())
        }

        Expression::Grouped { expr: inner_expr, .. } => {
            // Just evaluate the inner expression
            evaluate_expression_with_context(inner_expr, contract, mis)
        }

        // For Literal, Constant, and other types without ArgCalls, use standard evaluation
        _ => contract.evaluate_constant_expression(expr),
    }
}

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
    relax_jumps: bool,
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
                    Bytes::JumpPlaceholder(JumpPlaceholderData::new(
                        format!("goto_{}", &ir_macro.name),
                        PushOpcode::Push2,
                        format!("{}{}", Opcode::Jump, Opcode::Jumpdest),
                    )),
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

                // Resolve any ArgCall and ArgCallMacroInvocation arguments BEFORE pushing to scope
                // This ensures we check the calling macro's parameters, not the invoked macro's
                let mut resolved_args = Vec::new();
                for arg in &mi.args {
                    match arg {
                        MacroArg::ArgCall(arg_call) => {
                            // Resolve this ArgCall by looking it up in the current context
                            // BUT ensure proper scoping - the argument must be accessible from the current macro
                            let mut resolved = None;

                            // The current macro (the one calling this MacroInvocation) is macro_def
                            // It must have the argument as a parameter for it to be accessible
                            let current_has_param = macro_def.parameters.iter().any(|p| p.name.as_deref() == Some(&arg_call.name));

                            if !current_has_param {
                                // The current macro doesn't have this parameter, so the argument is not accessible
                                // This is a scoping violation - return an error instead of searching parent scopes
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingArgumentDefinition(arg_call.name.clone()),
                                    span: mi.span.clone_box(),
                                    token: None,
                                });
                            }

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
                                {
                                    // Check what type of argument value we have
                                    if let Some(arg_value) = parent_mi.args.get(param_idx) {
                                        match arg_value {
                                            MacroArg::Ident(macro_name) => {
                                                // Direct macro name reference
                                                resolved_macro_name = Some(macro_name.clone());
                                                break;
                                            }
                                            MacroArg::MacroCall(macro_call) => {
                                                // The argument is a macro invocation - extract the macro name
                                                resolved_macro_name = Some(macro_call.macro_name.clone());
                                                break;
                                            }
                                            _ => {
                                                // Other types not supported for macro invocation, continue searching
                                                continue;
                                            }
                                        }
                                    }
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
                        MacroArg::MacroCall(macro_call) => {
                            // Recursively resolve any ArgCalls in the macro call's arguments
                            let mut resolved_macro_args = Vec::new();
                            for macro_arg in &macro_call.args {
                                match macro_arg {
                                    MacroArg::ArgCall(arg_call) => {
                                        // Resolve this ArgCall by looking it up in the current context
                                        let mut resolved = None;
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
                                        resolved_macro_args.push(resolved.unwrap_or_else(|| macro_arg.clone()));
                                    }
                                    _ => resolved_macro_args.push(macro_arg.clone()),
                                }
                            }
                            // Create a new MacroCall with resolved arguments
                            resolved_args.push(MacroArg::MacroCall(MacroInvocation {
                                macro_name: macro_call.macro_name.clone(),
                                args: resolved_macro_args,
                                span: macro_call.span.clone(),
                            }));
                        }
                        _ => resolved_args.push(arg.clone()),
                    }
                }

                // Create a resolved invocation
                let resolved_mi = MacroInvocation { macro_name: mi.macro_name.clone(), args: resolved_args, span: mi.span.clone() };

                // Push the invoked macro to the scope AFTER resolving arguments
                // This is important for circular recursion detection
                scope.push(ir_macro);

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
                *offset += res.bytes.iter().map(|seg| seg.bytes.len()).sum::<usize>();
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
            bytes.push_with_offset(
                *offset,
                Bytes::JumpPlaceholder(JumpPlaceholderData::new(label.to_string(), PushOpcode::Push2, String::new())),
            );
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
                relax_jumps,
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
                relax_jumps,
            );
        }
        StatementType::IfStatement { condition, then_branch, else_if_branches, else_branch } => {
            // Evaluate condition with macro invocation context (supports <arg>)
            let condition_val = evaluate_expression_with_context(condition, contract, mis)?;
            let condition_true = !U256::from_be_bytes(condition_val).is_zero();

            // Select the appropriate branch
            let selected_branch = if condition_true {
                then_branch.as_slice()
            } else {
                // Check else if branches
                let mut matched_branch = None;
                for (else_if_condition, else_if_body) in else_if_branches {
                    let else_if_val = evaluate_expression_with_context(else_if_condition, contract, mis)?;
                    let else_if_true = !U256::from_be_bytes(else_if_val).is_zero();

                    if else_if_true {
                        matched_branch = Some(else_if_body.as_slice());
                        break;
                    }
                }

                // Use matched else if branch, or else branch, or empty
                matched_branch.or(else_branch.as_ref().map(|b| b.as_slice())).unwrap_or(&[])
            };

            // Convert branch statements to IRBytes first (like labels do)
            let branch_irbytes = MacroDefinition::to_irbytes(evm_version, selected_branch);

            // Process each IRByte through statement_gen or directly as bytes
            for ir_byte in branch_irbytes {
                match &ir_byte.ty {
                    IRByteType::Statement(stmt) => {
                        let (stmt_bytes, mut stmt_spans) = statement_gen(
                            evm_version,
                            stmt,
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
                            relax_jumps,
                        )?;
                        bytes.extend(stmt_bytes);
                        spans.append(&mut stmt_spans);
                    }
                    IRByteType::Bytes(b) => {
                        *offset += b.len();
                        bytes.push_with_offset(starting_offset, b.to_owned());
                        let span_info = if !ir_byte.span.0.is_empty() {
                            ir_byte.span.0.first().and_then(|sp| {
                                if sp.start != 0 || sp.end != 0 { contract.map_original_span_to_flattened(sp) } else { None }
                            })
                        } else {
                            None
                        };
                        spans.push(span_info);
                    }
                    IRByteType::Constant(name) => {
                        // Handle constant references
                        if let Some(push_value) = constant_gen(name, contract, ir_byte.span)? {
                            let push_bytes = push_value.to_hex_with_opcode(evm_version);
                            *offset += push_bytes.len() / 2;
                            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
                            let span_info = if !ir_byte.span.0.is_empty() {
                                ir_byte.span.0.first().and_then(|sp| {
                                    if sp.start != 0 || sp.end != 0 { contract.map_original_span_to_flattened(sp) } else { None }
                                })
                            } else {
                                None
                            };
                            spans.push(span_info);
                        }
                    }
                    IRByteType::ArgCall(parent_macro_name, arg_name) => {
                        // Handle arg calls
                        let bytes_before = bytes.len();
                        bubble_arg_call(
                            evm_version,
                            parent_macro_name.to_owned(),
                            arg_name.to_owned(),
                            &mut bytes,
                            macro_def,
                            contract,
                            scope,
                            offset,
                            mis,
                            jump_table,
                            table_instances,
                            utilized_tables,
                            ir_byte.span,
                            relax_jumps,
                        )?;
                        let bytes_added = bytes.len() - bytes_before;
                        let span_info = if !ir_byte.span.0.is_empty() {
                            ir_byte.span.0.first().and_then(|sp| {
                                if sp.start != 0 || sp.end != 0 { contract.map_original_span_to_flattened(sp) } else { None }
                            })
                        } else {
                            None
                        };
                        for _ in 0..bytes_added {
                            spans.push(span_info);
                        }
                    }
                }
            }
        }
        StatementType::ForLoop { variable, start, end, step, body } => {
            // Evaluate loop bounds with macro invocation context (supports <arg>)
            let start_val = evaluate_expression_with_context(start, contract, mis)?;
            let end_val = evaluate_expression_with_context(end, contract, mis)?;
            let step_val =
                if let Some(step_expr) = step { evaluate_expression_with_context(step_expr, contract, mis)? } else { str_to_bytes32("1") };

            // Convert to U256 for iteration
            let start_u256 = U256::from_be_bytes(start_val);
            let end_u256 = U256::from_be_bytes(end_val);
            let step_u256 = U256::from_be_bytes(step_val);

            if step_u256.is_zero() {
                return Err(CodegenError { kind: CodegenErrorKind::InvalidLoopStep, span: s.span.clone_box(), token: None });
            }

            // Maximum iterations safety check
            const MAX_LOOP_ITERATIONS: usize = 10_000;
            let mut current = start_u256;
            let mut iteration_count = 0;

            // Unroll the loop at compile-time
            while current < end_u256 {
                if iteration_count >= MAX_LOOP_ITERATIONS {
                    return Err(CodegenError {
                        kind: CodegenErrorKind::LoopIterationLimitExceeded(MAX_LOOP_ITERATIONS),
                        span: s.span.clone_box(),
                        token: None,
                    });
                }

                // Substitute loop variable in the body
                let current_bytes = current.to_be_bytes::<32>();
                let substituted_body = substitute_loop_variable_in_statements(body, variable, &current_bytes)?;

                // Convert substituted body to IRBytes first (like labels and if statements)
                let loop_irbytes = MacroDefinition::to_irbytes(evm_version, &substituted_body);

                // Process each IRByte
                for ir_byte in loop_irbytes {
                    match &ir_byte.ty {
                        IRByteType::Statement(stmt) => {
                            let (stmt_bytes, mut stmt_spans) = statement_gen(
                                evm_version,
                                stmt,
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
                                relax_jumps,
                            )?;
                            bytes.extend(stmt_bytes);
                            spans.append(&mut stmt_spans);
                        }
                        IRByteType::Bytes(b) => {
                            *offset += b.len();
                            bytes.push_with_offset(starting_offset, b.to_owned());
                            let span_info = if !ir_byte.span.0.is_empty() {
                                ir_byte.span.0.first().and_then(|sp| {
                                    if sp.start != 0 || sp.end != 0 { contract.map_original_span_to_flattened(sp) } else { None }
                                })
                            } else {
                                None
                            };
                            spans.push(span_info);
                        }
                        IRByteType::Constant(name) => {
                            if let Some(push_value) = constant_gen(name, contract, ir_byte.span)? {
                                let push_bytes = push_value.to_hex_with_opcode(evm_version);
                                *offset += push_bytes.len() / 2;
                                bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
                                let span_info = if !ir_byte.span.0.is_empty() {
                                    ir_byte.span.0.first().and_then(|sp| {
                                        if sp.start != 0 || sp.end != 0 { contract.map_original_span_to_flattened(sp) } else { None }
                                    })
                                } else {
                                    None
                                };
                                spans.push(span_info);
                            }
                        }
                        IRByteType::ArgCall(parent_macro_name, arg_name) => {
                            let bytes_before = bytes.len();
                            bubble_arg_call(
                                evm_version,
                                parent_macro_name.to_owned(),
                                arg_name.to_owned(),
                                &mut bytes,
                                macro_def,
                                contract,
                                scope,
                                offset,
                                mis,
                                jump_table,
                                table_instances,
                                utilized_tables,
                                ir_byte.span,
                                relax_jumps,
                            )?;
                            let bytes_added = bytes.len() - bytes_before;
                            let span_info = if !ir_byte.span.0.is_empty() {
                                ir_byte.span.0.first().and_then(|sp| {
                                    if sp.start != 0 || sp.end != 0 { contract.map_original_span_to_flattened(sp) } else { None }
                                })
                            } else {
                                None
                            };
                            for _ in 0..bytes_added {
                                spans.push(span_info);
                            }
                        }
                    }
                }

                current = current.saturating_add(step_u256);
                iteration_count += 1;
            }
        }
        sty => {
            tracing::error!(target: "codegen", "CURRENT MACRO DEF: {}", macro_def.name);
            tracing::error!(target: "codegen", "UNEXPECTED STATEMENT: {:?}", sty);
            return Err(CodegenError { kind: CodegenErrorKind::InvalidMacroStatement, span: s.span.clone_box(), token: None });
        }
    }

    Ok((bytes, spans))
}

/// Substitute loop variable in a list of statements
fn substitute_loop_variable_in_statements(
    statements: &[Statement],
    var_name: &str,
    value: &[u8; 32],
) -> Result<Vec<Statement>, CodegenError> {
    let mut substituted = Vec::new();

    for statement in statements {
        let new_statement = substitute_loop_variable_in_statement(statement, var_name, value)?;
        substituted.push(new_statement);
    }

    Ok(substituted)
}

/// Substitute loop variable in a single statement
fn substitute_loop_variable_in_statement(statement: &Statement, var_name: &str, value: &[u8; 32]) -> Result<Statement, CodegenError> {
    let new_ty = match &statement.ty {
        StatementType::Constant(name) => {
            // Check if this is a loop variable placeholder
            if name == &format!("__LOOP_VAR_{}", var_name) {
                // Replace with literal
                StatementType::Literal(*value)
            } else {
                statement.ty.clone()
            }
        }
        StatementType::Label(label) => {
            // Recursively substitute in label body and rename label to prevent collisions
            let substituted_inner = substitute_loop_variable_in_statements(&label.inner, var_name, value)?;
            let iter_value = U256::from_be_bytes(*value);
            let mut new_label = label.clone();
            new_label.inner = substituted_inner;
            new_label.name = format!("{}_{}", label.name, iter_value);
            StatementType::Label(new_label)
        }
        StatementType::LabelCall(label_name) => {
            // Update label references to match renamed labels
            let iter_value = U256::from_be_bytes(*value);
            StatementType::LabelCall(format!("{}_{}", label_name, iter_value))
        }
        StatementType::ForLoop { variable: loop_var, start, end, step, body } => {
            // Recursively substitute in nested for loop body
            let substituted_body = substitute_loop_variable_in_statements(body, var_name, value)?;
            StatementType::ForLoop {
                variable: loop_var.clone(),
                start: substitute_in_expression(start, var_name, value),
                end: substitute_in_expression(end, var_name, value),
                step: step.as_ref().map(|s| substitute_in_expression(s, var_name, value)),
                body: substituted_body,
            }
        }
        StatementType::IfStatement { condition, then_branch, else_if_branches, else_branch } => {
            // Recursively substitute in if statement branches
            let substituted_then = substitute_loop_variable_in_statements(then_branch, var_name, value)?;
            let substituted_else_if: Vec<_> = else_if_branches
                .iter()
                .map(|(cond, body)| {
                    Ok((substitute_in_expression(cond, var_name, value), substitute_loop_variable_in_statements(body, var_name, value)?))
                })
                .collect::<Result<Vec<_>, CodegenError>>()?;
            let substituted_else =
                else_branch.as_ref().map(|body| substitute_loop_variable_in_statements(body, var_name, value)).transpose()?;

            StatementType::IfStatement {
                condition: substitute_in_expression(condition, var_name, value),
                then_branch: substituted_then,
                else_if_branches: substituted_else_if,
                else_branch: substituted_else,
            }
        }
        _ => statement.ty.clone(),
    };

    Ok(Statement { ty: new_ty, span: statement.span.clone() })
}

/// Substitute loop variable in an expression
fn substitute_in_expression(expr: &Expression, var_name: &str, value: &[u8; 32]) -> Expression {
    match expr {
        Expression::Constant { name, span } => {
            // Check if this is a loop variable placeholder constant
            if name == &format!("__LOOP_VAR_{}", var_name) {
                Expression::Literal { value: *value, span: span.clone() }
            } else {
                expr.clone()
            }
        }
        Expression::Binary { left, op, right, span } => Expression::Binary {
            left: Box::new(substitute_in_expression(left, var_name, value)),
            op: op.clone(),
            right: Box::new(substitute_in_expression(right, var_name, value)),
            span: span.clone(),
        },
        Expression::Unary { op, expr: inner_expr, span } => {
            Expression::Unary { op: op.clone(), expr: Box::new(substitute_in_expression(inner_expr, var_name, value)), span: span.clone() }
        }
        _ => expr.clone(),
    }
}
