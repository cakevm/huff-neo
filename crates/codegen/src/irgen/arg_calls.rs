use crate::Codegen;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::prelude::*;
use std::str::FromStr;

/// Computes the scope context for label resolution in argument calls.
///
/// When a label is passed as an argument (e.g., `<label>` in a macro body), it must be
/// resolved from the calling macro's scope rather than the invoked macro's scope. This
/// ensures labels are accessible from their definition context when passed between macros.
///
/// # Arguments
/// * `scope` - Macro scope chain from root to current
/// * `mis` - Macro invocation stack with bytecode offsets
///
/// # Returns
/// * `(scope_path, scope_depth)` - Resolution context for label lookup
fn calculate_label_scope(scope: &[&MacroDefinition], mis: &[(usize, MacroInvocation)]) -> (Vec<String>, usize) {
    // Standard case: search from parent's scope
    // This works for most cases including nested arguments
    let scope_path = if scope.len() > 2 {
        let parent_offset = if mis.len() > 1 { mis[mis.len() - 2].0 } else { 0 };
        let mut path: Vec<String> = scope[..scope.len() - 2].iter().map(|m| m.name.clone()).collect();
        path.push(format!("{}_{}", scope[scope.len() - 2].name, parent_offset));
        path
    } else if scope.len() > 1 {
        scope[..scope.len() - 1].iter().map(|m| m.name.clone()).collect()
    } else {
        vec![]
    };
    let scope_depth = scope.len().saturating_sub(2);
    (scope_path, scope_depth)
}
// Arguments can be literals, labels, opcodes, constants, or macro calls
// !! IF THERE IS AMBIGUOUS NOMENCLATURE
// !! (E.G. BOTH OPCODE AND LABEL ARE THE SAME STRING)
// !! COMPILATION _WILL_ ERROR

/// Arg Call Bubbling
#[allow(clippy::too_many_arguments)]
pub fn bubble_arg_call(
    evm_version: &EVMVersion,
    arg_parent_macro_name: String,
    arg_name: String,
    bytes: &mut Vec<(usize, Bytes)>,
    macro_def: &MacroDefinition,
    contract: &Contract,
    scope: &mut [&MacroDefinition],
    offset: &mut usize,
    // mis: Parent macro invocations and their indices
    mis: &mut [(usize, MacroInvocation)],
    jump_table: &mut JumpTable,
    table_instances: &mut Jumps,
    utilized_tables: &mut Vec<TableDefinition>,
    span: &AstSpan,
) -> Result<(), CodegenError> {
    tracing::debug!(
        target: "codegen",
        "bubble_arg_call: {} -> {}, [{:?}]",
        arg_parent_macro_name,
        arg_name,
        scope.iter().map(|m| m.name.clone()).collect::<Vec<_>>()
    );

    let starting_offset = *offset;

    if let Some(macro_invoc) = mis.last() {
        // Literal, Ident & Arg Call Check
        // First get this arg_name position in the correct macro definition params
        // We need to use the macro that corresponds to the arg_parent_macro_name, not the current macro_def
        let target_macro = if arg_parent_macro_name == macro_def.name {
            macro_def
        } else {
            // Find the macro that corresponds to the arg_parent_macro_name
            scope.iter().find(|m| m.name == arg_parent_macro_name).map_or(macro_def, |v| v)
        };

        // Find the macro invocation that corresponds to the target macro
        let target_macro_invoc = if target_macro.name == macro_invoc.1.macro_name {
            macro_invoc
        } else {
            // Find the invocation for the target macro in the mis stack
            mis.iter().rev().find(|(_, mi)| mi.macro_name == target_macro.name).unwrap_or(macro_invoc)
        };

        if let Some(pos) = target_macro.parameters.iter().position(|r| r.name.as_ref().is_some_and(|s| s.eq(&arg_name))) {
            tracing::info!(target: "codegen", "GOT \"{}\" POS IN ARG LIST: {} (from macro {} invocation {})", arg_name, pos, target_macro.name, target_macro_invoc.1.macro_name);

            if let Some(arg) = target_macro_invoc.1.args.get(pos) {
                // Special case: if the argument is an ArgCallMacroInvocation, expand it here
                if let MacroArg::ArgCallMacroInvocation(inner_arg_name, invoc_args) = arg {
                    tracing::info!(target: "codegen", "GOT ArgCallMacroInvocation for argument '{}', expanding it", inner_arg_name);

                    // We need to resolve inner_arg_name and expand the macro
                    // Look in the parent scope for the actual macro name
                    if scope.len() > 1 {
                        let parent_macro = scope[scope.len() - 2];
                        if let Some(parent_param_idx) = parent_macro.parameters.iter().position(|p| p.name.as_ref() == Some(inner_arg_name))
                            && mis.len() > 1
                            && let Some(parent_invoc) = mis.get(mis.len() - 2)
                            && let Some(MacroArg::Ident(actual_macro_name)) = parent_invoc.1.args.get(parent_param_idx)
                            && let Some(called_macro) = contract.find_macro_by_name(actual_macro_name)
                        {
                            let inner_mi =
                                MacroInvocation { macro_name: actual_macro_name.clone(), args: invoc_args.clone(), span: span.clone() };

                            let mut new_scope = scope.to_vec();
                            new_scope.push(called_macro);
                            let mut new_mis = mis.to_vec();
                            new_mis.push((*offset, inner_mi));

                            match Codegen::macro_to_bytecode(
                                evm_version,
                                called_macro,
                                contract,
                                &mut new_scope,
                                *offset,
                                &mut new_mis,
                                false,
                                None,
                            ) {
                                Ok(expanded) => {
                                    let byte_len: usize = expanded.bytes.iter().map(|(_, b)| b.0.len() / 2).sum();
                                    bytes.extend(expanded.bytes);
                                    *offset += byte_len;

                                    // Handle jumps and tables
                                    for jump in expanded.unmatched_jumps {
                                        if let Some(existing) = jump_table.get_mut(&jump.bytecode_index) {
                                            existing.push(jump);
                                        } else {
                                            jump_table.insert(jump.bytecode_index, vec![jump]);
                                        }
                                    }
                                    for table_instance in expanded.table_instances {
                                        table_instances.push(table_instance);
                                    }
                                    for table in expanded.utilized_tables {
                                        if !utilized_tables.contains(&table) {
                                            utilized_tables.push(table);
                                        }
                                    }
                                    return Ok(());
                                }
                                Err(e) => return Err(e),
                            }
                        }
                    }
                    // If we couldn't expand it, fall through to normal handling
                }

                // Original handling for other argument types
                tracing::info!(target: "codegen", "GOT \"{:?}\" ARG FROM MACRO INVOCATION", arg);
                match arg {
                    MacroArg::Literal(l) => {
                        tracing::info!(target: "codegen", "GOT LITERAL {} ARG FROM MACRO INVOCATION", bytes32_to_hex_string(l, false));
                        let push_bytes = literal_gen(evm_version, l);
                        *offset += push_bytes.len() / 2;
                        bytes.push((starting_offset, Bytes(push_bytes)));
                    }
                    MacroArg::Opcode(o) => {
                        tracing::info!(target: "codegen", "GOT \"{:?}\" OPCODE FROM MACRO INVOCATION", o);
                        let b = Bytes(o.to_string());
                        *offset += b.0.len() / 2;
                        bytes.push((starting_offset, b));
                    }
                    MacroArg::ArgCall(ArgCall { macro_name: ac_parent_macro_name, name: ac, span: arg_span }) => {
                        tracing::info!(target: "codegen", "GOT ARG CALL \"{}\" ARG FROM MACRO INVOCATION", ac);
                        tracing::debug!(target: "codegen", "~~~ BUBBLING UP ARG CALL");
                        let scope_len = scope.len();
                        let new_scope = &mut scope[..scope_len.saturating_sub(1)];
                        let bubbled_macro_invocation = new_scope.last().unwrap();
                        tracing::debug!(target: "codegen", "BUBBLING UP WITH MACRO DEF: {}", &bubbled_macro_invocation.name);
                        tracing::debug!(target: "codegen", "CURRENT MACRO DEF: {}", macro_def.name);

                        // Only remove an invocation if not at bottom level, otherwise we'll
                        // remove one too many
                        let last_mi = match mis.last() {
                            Some(mi) => mi,
                            None => {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingMacroInvocation(macro_def.name.clone()),
                                    span: bubbled_macro_invocation.span.clone(),
                                    token: None,
                                });
                            }
                        };
                        let mis_len = mis.len();
                        return if last_mi.1.macro_name.eq(&macro_def.name) {
                            bubble_arg_call(
                                evm_version,
                                ac_parent_macro_name.clone(),
                                ac.clone(),
                                bytes,
                                bubbled_macro_invocation,
                                contract,
                                new_scope,
                                offset,
                                &mut mis[..mis_len.saturating_sub(1)].to_vec(),
                                jump_table,
                                table_instances,
                                utilized_tables,
                                arg_span,
                            )
                        } else {
                            bubble_arg_call(
                                evm_version,
                                ac_parent_macro_name.clone(),
                                ac.clone(),
                                bytes,
                                bubbled_macro_invocation,
                                contract,
                                new_scope,
                                offset,
                                mis,
                                jump_table,
                                table_instances,
                                utilized_tables,
                                span,
                            )
                        };
                    }
                    MacroArg::Ident(iden) => {
                        tracing::debug!(target: "codegen", "Found MacroArg::Ident IN \"{}\" Macro Invocation: \"{}\"!", target_macro_invoc.1.macro_name, iden);

                        // Check for a constant first
                        if let Some(constant) = contract
                            .constants
                            .lock()
                            .map_err(|_| CodegenError::new(CodegenErrorKind::LockingError, AstSpan(vec![]), None))?
                            .iter()
                            .find(|const_def| const_def.name.eq(iden))
                        {
                            tracing::info!(target: "codegen", "ARGCALL IS CONSTANT: {:?}", constant);
                            let push_bytes = match &constant.value {
                                ConstVal::Bytes(bytes) => {
                                    let hex_literal: String = bytes.clone().0;
                                    format!("{:02x}{hex_literal}", 95 + hex_literal.len() / 2)
                                }
                                ConstVal::StoragePointer(sp) => {
                                    let hex_literal: String = bytes32_to_hex_string(sp, false);
                                    format!("{:02x}{hex_literal}", 95 + hex_literal.len() / 2)
                                }
                                ConstVal::BuiltinFunctionCall(bf) => {
                                    Codegen::gen_builtin_bytecode(evm_version, contract, bf, target_macro_invoc.1.span.clone())?
                                }
                                ConstVal::FreeStoragePointer(fsp) => {
                                    // If this is reached in codegen stage,
                                    // `derive_storage_pointers`
                                    // method was not called on the AST.
                                    tracing::error!(target: "codegen", "STORAGE POINTERS INCORRECTLY DERIVED FOR \"{:?}\"", fsp);
                                    return Err(CodegenError {
                                        kind: CodegenErrorKind::StoragePointersNotDerived,
                                        span: AstSpan(vec![]),
                                        token: None,
                                    });
                                }
                            };
                            *offset += push_bytes.len() / 2;
                            tracing::info!(target: "codegen", "OFFSET: {}, PUSH BYTES: {:?}", offset, push_bytes);
                            bytes.push((starting_offset, Bytes(push_bytes)));
                        } else if let Ok(o) = Opcode::from_str(iden) {
                            tracing::debug!(target: "codegen", "Found Opcode: {}", o);
                            let b = Bytes(o.to_string());
                            *offset += b.0.len() / 2;
                            bytes.push((starting_offset, b));
                        } else {
                            tracing::debug!(target: "codegen", "Found Label Call: {}", iden);

                            // This should be equivalent to a label call.
                            bytes.push((*offset, Bytes(format!("{}xxxx", Opcode::Push2))));

                            let (scope_path, scope_depth) = calculate_label_scope(scope, mis);
                            jump_table.insert(
                                *offset,
                                vec![Jump {
                                    label: iden.to_owned(),
                                    bytecode_index: 0,
                                    span: target_macro_invoc.1.span.clone(),
                                    scope_depth,
                                    scope_path,
                                }],
                            );
                            *offset += 3;
                        }
                    }
                    MacroArg::ArgCallMacroInvocation(arg_name, invoc_args) => {
                        // Resolve the argument to get the actual macro name
                        if let Some(param_idx) = target_macro.parameters.iter().position(|p| p.name.as_ref() == Some(arg_name)) {
                            if let Some(MacroArg::Ident(actual_macro_name)) = target_macro_invoc.1.args.get(param_idx) {
                                // Now invoke the resolved macro with the provided arguments
                                if let Some(called_macro) = contract.find_macro_by_name(actual_macro_name) {
                                    let inner_mi = MacroInvocation {
                                        macro_name: actual_macro_name.clone(),
                                        args: invoc_args.clone(),
                                        span: AstSpan(vec![]), // We don't have the original span here
                                    };

                                    let mut new_scope = scope.to_vec();
                                    new_scope.push(called_macro);
                                    let mut new_mis = mis.to_vec();
                                    new_mis.push((*offset, inner_mi));

                                    match Codegen::macro_to_bytecode(
                                        evm_version,
                                        called_macro,
                                        contract,
                                        &mut new_scope,
                                        *offset,
                                        &mut new_mis,
                                        false,
                                        None,
                                    ) {
                                        Ok(expanded_macro) => {
                                            let byte_len: usize = expanded_macro.bytes.iter().map(|(_, b)| b.0.len() / 2).sum();
                                            bytes.extend(expanded_macro.bytes);
                                            *offset += byte_len;

                                            // Bubble up jumps and tables
                                            for unmatched_jump in expanded_macro.unmatched_jumps {
                                                let existing_jumps =
                                                    jump_table.get(&unmatched_jump.bytecode_index).cloned().unwrap_or_else(Vec::new);
                                                let mut new_jumps = existing_jumps;
                                                new_jumps.push(unmatched_jump.clone());
                                                jump_table.insert(unmatched_jump.bytecode_index, new_jumps);
                                            }
                                            for table_instance in expanded_macro.table_instances {
                                                table_instances.push(table_instance);
                                            }
                                            for table in expanded_macro.utilized_tables {
                                                if !utilized_tables.contains(&table) {
                                                    utilized_tables.push(table);
                                                }
                                            }
                                        }
                                        Err(e) => return Err(e),
                                    }
                                } else {
                                    return Err(CodegenError {
                                        kind: CodegenErrorKind::MissingMacroDefinition(actual_macro_name.clone()),
                                        span: span.clone(),
                                        token: None,
                                    });
                                }
                            } else {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingArgumentDefinition(arg_name.clone()),
                                    span: span.clone(),
                                    token: None,
                                });
                            }
                        } else {
                            return Err(CodegenError {
                                kind: CodegenErrorKind::MissingArgumentDefinition(arg_name.clone()),
                                span: span.clone(),
                                token: None,
                            });
                        }
                    }
                    MacroArg::MacroCall(inner_mi) => {
                        if !inner_mi.args.is_empty() {
                            // This is an inline expansion case (macro with arguments)
                            tracing::debug!(target: "codegen", "Processing inline MacroCall argument: {}", inner_mi.macro_name);

                            if let Some(called_macro) = contract.find_macro_by_name(&inner_mi.macro_name) {
                                let mut new_scope = scope.to_vec();
                                new_scope.push(called_macro);
                                let mut new_mis = mis.to_vec();
                                new_mis.push((*offset, inner_mi.clone()));

                                match Codegen::macro_to_bytecode(
                                    evm_version,
                                    called_macro,
                                    contract,
                                    &mut new_scope,
                                    *offset,
                                    &mut new_mis,
                                    false,
                                    None,
                                ) {
                                    Ok(expanded_macro) => {
                                        let byte_len: usize = expanded_macro.bytes.iter().map(|(_, b)| b.0.len() / 2).sum();
                                        bytes.extend(expanded_macro.bytes);
                                        *offset += byte_len;

                                        // Bubble up unmatched jumps
                                        for unmatched_jump in expanded_macro.unmatched_jumps {
                                            let existing_jumps =
                                                jump_table.get(&unmatched_jump.bytecode_index).cloned().unwrap_or_else(Vec::new);
                                            let mut new_jumps = existing_jumps;
                                            new_jumps.push(unmatched_jump.clone());
                                            jump_table.insert(unmatched_jump.bytecode_index, new_jumps);
                                        }

                                        // Bubble up table instances
                                        for table_instance in expanded_macro.table_instances {
                                            table_instances.push(table_instance.clone());
                                        }

                                        // Bubble up utilized tables
                                        for table in expanded_macro.utilized_tables {
                                            if !utilized_tables.contains(&table) {
                                                utilized_tables.push(table);
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            } else {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingMacroDefinition(inner_mi.macro_name.clone()),
                                    span: inner_mi.span.clone(),
                                    token: None,
                                });
                            }
                        } else {
                            // This is a parameter substitution case (macro without arguments)
                            // Process it normally for substitution where <arg> appears
                            tracing::debug!(target: "codegen", "Processing parameter substitution MacroCall: {}", inner_mi.macro_name);

                            if let Some(called_macro) = contract.find_macro_by_name(&inner_mi.macro_name) {
                                tracing::debug!(target: "codegen", "Found valid macro: {}", called_macro.name);

                                let mut new_scope = scope.to_vec();
                                new_scope.push(called_macro);
                                let mut new_mis = mis.to_vec();
                                new_mis.push((*offset, inner_mi.clone()));

                                match Codegen::macro_to_bytecode(
                                    evm_version,
                                    called_macro,
                                    contract,
                                    &mut new_scope,
                                    *offset,
                                    &mut new_mis,
                                    false,
                                    None,
                                ) {
                                    Ok(expanded_macro) => {
                                        let byte_len: usize = expanded_macro.bytes.iter().map(|(_, b)| b.0.len() / 2).sum();
                                        bytes.extend(expanded_macro.bytes);
                                        *offset += byte_len;

                                        // Bubble up unmatched jumps from the expanded macro to the parent scope
                                        // This is crucial for label resolution across macro boundaries
                                        for unmatched_jump in expanded_macro.unmatched_jumps {
                                            // Add the unmatched jump to the parent's jump table
                                            let existing_jumps =
                                                jump_table.get(&unmatched_jump.bytecode_index).cloned().unwrap_or_else(Vec::new);
                                            let mut new_jumps = existing_jumps;
                                            new_jumps.push(unmatched_jump.clone());
                                            jump_table.insert(unmatched_jump.bytecode_index, new_jumps);

                                            tracing::debug!(target: "codegen", "Bubbled up unmatched jump for label '{}' at index {} from MacroCall", 
                                                           unmatched_jump.label, unmatched_jump.bytecode_index);
                                        }

                                        // Bubble up table instances from the expanded macro to the parent scope
                                        // This is crucial for table resolution across macro boundaries
                                        for table_instance in expanded_macro.table_instances {
                                            table_instances.push(table_instance.clone());

                                            tracing::debug!(target: "codegen", "Bubbled up table instance for label '{}' at index {} from MacroCall", 
                                                           table_instance.label, table_instance.bytecode_index);
                                        }

                                        // Bubble up utilized tables from the expanded macro to the parent scope
                                        for table in expanded_macro.utilized_tables {
                                            if !utilized_tables.contains(&table) {
                                                utilized_tables.push(table);
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            } else {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingMacroDefinition(inner_mi.macro_name.clone()),
                                    span: inner_mi.span.clone(),
                                    token: None,
                                });
                            }
                        }
                    }
                }
            } else {
                tracing::warn!(target: "codegen", "\"{}\" FOUND IN MACRO DEF \"{}\" BUT NOT IN MACRO INVOCATION \"{}\"!", arg_name, target_macro.name, target_macro_invoc.1.macro_name);
            }
        } else {
            // Argument not found in current macro parameters.
            // Try to bubble up to parent scope to find it.
            tracing::debug!(target: "codegen", "Argument \"{}\" not found in macro \"{}\", attempting to bubble up", arg_name, macro_def.name);

            if scope.len() > 1 && mis.len() > 1 {
                // We have a parent scope to bubble up to
                let scope_len = scope.len();
                let new_scope = &mut scope[..scope_len.saturating_sub(1)];
                let bubbled_macro_invocation = new_scope.last().unwrap();
                tracing::debug!(target: "codegen", "BUBBLING UP TO MACRO DEF: {}", &bubbled_macro_invocation.name);

                let mis_len = mis.len();
                // Remove the current mis entry since we're bubbling up
                let new_mis = &mut mis[..mis_len.saturating_sub(1)].to_vec();

                return bubble_arg_call(
                    evm_version,
                    arg_parent_macro_name,
                    arg_name,
                    bytes,
                    bubbled_macro_invocation,
                    contract,
                    new_scope,
                    offset,
                    new_mis,
                    jump_table,
                    table_instances,
                    utilized_tables,
                    span,
                );
            } else {
                // No parent scope or at the top level - this is truly an undefined argument
                return Err(CodegenError {
                    kind: CodegenErrorKind::MissingArgumentDefinition(arg_name.to_string()),
                    span: span.clone(),
                    token: None,
                });
            }
        }
    } else {
        // This is a label call
        tracing::info!(target: "codegen", "RECURSE_BYTECODE ARG CALL DEFAULTING TO LABEL CALL: \"{}\"", arg_name);
        let new_span = match mis.last() {
            Some(mi) => mi.1.span.clone(),
            None => AstSpan(vec![]),
        };

        let (scope_path, scope_depth) = calculate_label_scope(scope, mis);
        jump_table.insert(
            mis.last().map(|mi| mi.0).unwrap_or_else(|| 0),
            vec![Jump { label: arg_name.to_owned(), bytecode_index: 0, span: new_span, scope_depth, scope_path }],
        );
        bytes.push((*offset, Bytes(format!("{}xxxx", Opcode::Push2))));
        *offset += 3;
    }

    Ok(())
}
