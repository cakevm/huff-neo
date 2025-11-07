use crate::{Codegen, MAX_MACRO_RECURSION_DEPTH};
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::bytecode::{BytecodeSegments, JumpPlaceholderData, PushOpcode};
use huff_neo_utils::prelude::*;
use huff_neo_utils::scope::ScopeManager;
use std::str::FromStr;

/// Maximum iterations allowed when resolving argument references through the scope chain.
/// Prevents infinite loops when circular argument references exist (e.g., arg1 -> arg2 -> arg1).
const MAX_ARG_RESOLUTION_ITERATIONS: usize = 100;

/// Maximum iterations for the argument bubbling loop in `bubble_arg_call`.
/// Prevents infinite loops when bubbling arguments up through nested macro calls.
const MAX_BUBBLE_ARG_ITERATIONS: usize = 1000;

/// Resolves an argument reference to the actual macro name it represents.
///
/// Searches through the macro call stack to find where the argument is defined and
/// follows any chained argument references until reaching a concrete macro name.
///
/// # Arguments
/// * `arg_name` - The argument to resolve (e.g., "foo" from `<foo>`)
/// * `scope_mgr` - Call stack context for argument lookup
/// * `max_depth` - How far up the call stack to search
/// * `contract` - Source of macro definitions
///
/// # Returns
/// The resolved macro name, or an error if the argument is undefined or invalid.
fn resolve_argument_through_scope<'a>(
    arg_name: &str,
    scope_mgr: &ScopeManager<'a>,
    max_depth: usize,
    contract: &Contract,
) -> Result<String, CodegenErrorKind> {
    let mut current_arg_name = arg_name.to_string();
    let mut iteration_count = 0;

    // Iterative resolution to prevent stack overflow
    loop {
        iteration_count += 1;
        if iteration_count > MAX_ARG_RESOLUTION_ITERATIONS {
            return Err(CodegenErrorKind::ArgResolutionLimitExceeded(current_arg_name, MAX_ARG_RESOLUTION_ITERATIONS));
        }

        tracing::debug!(target: "codegen", "Resolving argument '{}' through scope up to depth {} (iteration {})", current_arg_name, max_depth, iteration_count);

        // Search through the frames in reverse (from deepest to root, up to max_depth)
        let mut found = false;
        for depth in (0..max_depth.min(scope_mgr.depth())).rev() {
            if let Some(frame) = scope_mgr.frames.get(depth)
                && let Some(ref invocation) = frame.invocation
            {
                tracing::debug!(target: "codegen", "Checking frame at depth {} for macro '{}'", depth, invocation.macro_name);

                // Find the macro definition
                if let Some(invoked_macro) = contract.find_macro_by_name(&invocation.macro_name) {
                    // Check if this macro has our argument
                    if let Some(param_index) = invoked_macro.parameters.iter().position(|p| p.name.as_deref() == Some(&current_arg_name)) {
                        tracing::debug!(target: "codegen", "Found parameter '{}' at index {} in macro '{}'", current_arg_name, param_index, invocation.macro_name);

                        // Get the corresponding argument value
                        if let Some(arg_value) = invocation.args.get(param_index) {
                            match arg_value {
                                MacroArg::Ident(macro_name) => {
                                    // Direct macro name - we're done
                                    tracing::info!(target: "codegen", "Resolved '{}' to macro: {}", current_arg_name, macro_name);
                                    return Ok(macro_name.clone());
                                }
                                MacroArg::ArgCall(arg_call) => {
                                    // The argument is itself an argument reference - continue iterating
                                    tracing::debug!(target: "codegen", "Argument '{}' references another argument '{}', continuing resolution", current_arg_name, arg_call.name);
                                    current_arg_name = arg_call.name.clone();
                                    found = true;
                                    break; // Break inner loop to restart search with new arg name
                                }
                                MacroArg::ArgCallMacroInvocation(inner_arg_name, _) => {
                                    // The argument is an invocation of another argument - resolve that argument
                                    tracing::debug!(target: "codegen", "Argument '{}' is a macro invocation of argument '{}', continuing resolution", current_arg_name, inner_arg_name);
                                    current_arg_name = inner_arg_name.clone();
                                    found = true;
                                    break; // Break inner loop to restart search with new arg name
                                }
                                MacroArg::Noop => {
                                    // __NOOP cannot be invoked as a macro
                                    return Err(CodegenErrorKind::InvalidMacroArgumentType(format!(
                                        "Cannot invoke __NOOP as a macro in argument '{}'",
                                        current_arg_name
                                    )));
                                }
                                _ => {
                                    // Other types of arguments not supported for macro invocation
                                    tracing::debug!(target: "codegen", "Argument type {:?} not supported for macro invocation, continuing search", arg_value);
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }

        // If we didn't find anything that requires continuing the search, we're done
        if !found {
            tracing::error!(target: "codegen", "Failed to resolve argument '{}' in scope up to depth {}", current_arg_name, max_depth);
            return Err(CodegenErrorKind::MissingArgumentDefinition(current_arg_name));
        }
    }
}

// Arguments can be literals, labels, opcodes, constants, or macro calls
// !! IF THERE IS AMBIGUOUS NOMENCLATURE
// !! (E.G. BOTH OPCODE AND LABEL ARE THE SAME STRING)
// !! COMPILATION _WILL_ ERROR

/// Resolves a macro argument reference and generates its bytecode.
///
/// When a macro body contains `<arg>`, this resolves what `arg` refers to (opcode, label,
/// literal, or another macro call) and generates the appropriate bytecode.
#[allow(clippy::too_many_arguments)]
pub fn bubble_arg_call<'a>(
    evm_version: &EVMVersion,
    mut arg_parent_macro_name: String,
    mut arg_name: String,
    bytes: &mut BytecodeSegments,
    mut macro_def: &'a MacroDefinition,
    contract: &'a Contract,
    scope_mgr: &mut ScopeManager<'a>,
    offset: &mut usize,
    jump_table: &mut JumpTable,
    table_instances: &mut Jumps,
    utilized_tables: &mut Vec<TableDefinition>,
    span: &AstSpan,
    relax_jumps: bool,
) -> Result<(), CodegenError> {
    tracing::debug!(target: "codegen", "bubble_arg_call: arg_name={}, macro_def={}", arg_name, macro_def.name);

    // Safety check to prevent infinite loops
    if scope_mgr.depth() > MAX_MACRO_RECURSION_DEPTH {
        let macro_chain = scope_mgr.macro_stack().iter().map(|m| m.name.as_str()).collect::<Vec<_>>().join(" -> ");
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidMacroArgumentType(format!(
                "Maximum macro recursion depth ({}) exceeded while resolving argument '{}' in macro '{}'.\nMacro call chain: {}\nThis likely indicates circular macro invocations or infinitely recursive argument resolution.",
                MAX_MACRO_RECURSION_DEPTH, arg_name, arg_parent_macro_name, macro_chain
            )),
            span: span.clone_box(),
            token: None,
        });
    }

    let starting_offset = *offset;

    // Start with full depth for searching
    let mut search_depth = scope_mgr.depth();

    let mut iteration_count = 0;

    // Iterative loop instead of recursion
    loop {
        iteration_count += 1;
        if iteration_count > MAX_BUBBLE_ARG_ITERATIONS {
            return Err(CodegenError {
                kind: CodegenErrorKind::BubbleArgLimitExceeded(arg_name.to_string(), MAX_BUBBLE_ARG_ITERATIONS),
                span: span.clone_box(),
                token: None,
            });
        }

        tracing::debug!(
            target: "codegen",
            "bubble_arg_call iteration {}: {} -> {}, search_depth={}, current_macro={}",
            iteration_count,
            arg_parent_macro_name,
            arg_name,
            search_depth,
            macro_def.name
        );

        // Get the current macro's invocation for looking up arguments in this macro
        let current_invoc = scope_mgr.current_invocation();
        if let Some(current_macro_invoc) = current_invoc {
            let macro_invoc_tuple = (starting_offset, current_macro_invoc.clone());

            // First get this arg_name position in the correct macro definition params
            // We need to use the macro that corresponds to the arg_parent_macro_name, not the current macro_def
            let target_macro = if arg_parent_macro_name == macro_def.name {
                macro_def
            } else {
                // Find the macro that corresponds to the arg_parent_macro_name
                scope_mgr.macro_stack().iter().find(|m| m.name == arg_parent_macro_name).map_or(macro_def, |v| v)
            };

            // Find the macro invocation that corresponds to the target macro
            // Search through parent frames (using search_depth to respect exclusions)
            let target_macro_invoc = if target_macro.name == macro_invoc_tuple.1.macro_name {
                macro_invoc_tuple.clone()
            } else {
                // Search parent frames for the target macro's invocation, respecting search_depth
                scope_mgr.frames[..search_depth.min(scope_mgr.depth())]
                    .iter()
                    .rev()
                    .find_map(|frame| {
                        frame
                            .invocation
                            .as_ref()
                            .and_then(|inv| if inv.macro_name == target_macro.name { Some((frame.offset, inv.clone())) } else { None })
                    })
                    .unwrap_or(macro_invoc_tuple.clone())
            };

            if let Some(pos) = target_macro.parameters.iter().position(|r| r.name.as_ref().is_some_and(|s| s.eq(&arg_name))) {
                tracing::debug!(target: "codegen", "GOT \"{}\" POS IN ARG LIST: {} (from macro {} invocation {})", arg_name, pos, target_macro.name, target_macro_invoc.1.macro_name);

                if let Some(arg) = target_macro_invoc.1.args.get(pos) {
                    tracing::debug!(target: "codegen", "ARG VALUE: {:?}", arg);

                    // Special case: if the argument is an ArgCallMacroInvocation, expand it here
                    if let MacroArg::ArgCallMacroInvocation(inner_arg_name, invoc_args) = arg {
                        tracing::info!(target: "codegen", "GOT ArgCallMacroInvocation for argument '{}', expanding it", inner_arg_name);

                        // We need to resolve inner_arg_name to get the actual macro name
                        // Use search_depth which may be reduced to exclude problematic frames
                        match resolve_argument_through_scope(inner_arg_name, scope_mgr, search_depth, contract) {
                            Ok(actual_macro_name) => {
                                if let Some(called_macro) = contract.find_macro_by_name(&actual_macro_name) {
                                    let inner_mi = MacroInvocation {
                                        macro_name: actual_macro_name.clone(),
                                        args: invoc_args.clone(),
                                        span: span.clone(),
                                    };

                                    scope_mgr.push_macro_with_invocation(called_macro, *offset, inner_mi);
                                    let result = Codegen::macro_to_bytecode(
                                        evm_version,
                                        called_macro,
                                        contract,
                                        scope_mgr,
                                        *offset,
                                        false,
                                        None,
                                        relax_jumps,
                                    );
                                    // Note: macro_to_bytecode already pops the macro from scope_mgr
                                    match result {
                                        Ok(expanded) => {
                                            let byte_len: usize = expanded.bytes.iter().map(|seg| seg.bytes.len()).sum();
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
                                } else {
                                    return Err(CodegenError {
                                        kind: CodegenErrorKind::MissingMacroDefinition(actual_macro_name),
                                        span: span.clone_box(),
                                        token: None,
                                    });
                                }
                            }
                            Err(e) => {
                                // Could not resolve the argument
                                return Err(CodegenError { kind: e, span: span.clone_box(), token: None });
                            }
                        }
                    }

                    // Original handling for other argument types
                    tracing::info!(target: "codegen", "GOT \"{:?}\" ARG FROM MACRO INVOCATION", arg);
                    match arg {
                        MacroArg::Literal(l) => {
                            tracing::info!(target: "codegen", "GOT LITERAL {} ARG FROM MACRO INVOCATION", bytes32_to_hex_string(l, false));
                            let push_bytes = literal_gen(evm_version, l);
                            *offset += push_bytes.len() / 2;
                            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
                        }
                        MacroArg::Opcode(o) => {
                            tracing::info!(target: "codegen", "GOT \"{:?}\" OPCODE FROM MACRO INVOCATION", o);
                            let b = Bytes::Raw(o.to_string());
                            *offset += b.len();
                            bytes.push_with_offset(starting_offset, b);
                        }
                        MacroArg::Noop => {
                            tracing::info!(target: "codegen", "GOT __NOOP ARG FROM MACRO INVOCATION - GENERATING NO BYTECODE");
                            // Generate no bytecode - __NOOP is a no-op
                        }
                        MacroArg::BuiltinFunctionCall(bf) => {
                            tracing::info!(target: "codegen", "GOT BUILTIN FUNCTION CALL {:?} ARG FROM MACRO INVOCATION", bf.kind);
                            let push_value = Codegen::gen_builtin_bytecode(contract, bf, bf.span.clone())?;

                            // For __RIGHTPAD, always use PUSH32 with full 32 bytes (no optimization)
                            let push_bytes = match bf.kind {
                                BuiltinFunctionKind::RightPad => push_value.to_hex_full_with_opcode(),
                                _ => push_value.to_hex_with_opcode(evm_version),
                            };

                            *offset += push_bytes.len() / 2;
                            bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
                        }
                        MacroArg::ArgCall(ArgCall { macro_name: ac_parent_macro_name, name: ac, span: _arg_span }) => {
                            tracing::info!(target: "codegen", "GOT ARG CALL \"{}\" ARG FROM MACRO INVOCATION", ac);
                            tracing::debug!(target: "codegen", "~~~ BUBBLING UP ARG CALL");
                            let parent_scope_id = scope_mgr.parent_scope();
                            let bubbled_macro_exists = scope_mgr.macro_at_depth(parent_scope_id.depth).is_some();
                            tracing::debug!(target: "codegen", "BUBBLING UP WITH MACRO EXISTS: {}", bubbled_macro_exists);
                            tracing::debug!(target: "codegen", "CURRENT MACRO DEF: {}", macro_def.name);

                            // Verify we have a parent invocation to bubble from
                            if search_depth <= 1 {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingMacroInvocation(macro_def.name.to_string()),
                                    span: span.clone_box(),
                                    token: None,
                                });
                            }

                            if !bubbled_macro_exists {
                                return Err(CodegenError {
                                    kind: CodegenErrorKind::MissingMacroInvocation(macro_def.name.to_string()),
                                    span: span.clone_box(),
                                    token: None,
                                });
                            }

                            // Get parent macro
                            let bubbled_macro_def = scope_mgr.macro_at_depth(parent_scope_id.depth).ok_or_else(|| CodegenError {
                                kind: CodegenErrorKind::MissingMacroInvocation(macro_def.name.to_string()),
                                span: span.clone_box(),
                                token: None,
                            })?;

                            // Check if current invocation matches current macro (prevents infinite loops)
                            // This replicates: if last_mi.1.macro_name.eq(&macro_def.name)
                            let should_exclude_current_frame = if let Some(current_inv) = scope_mgr.current_invocation() {
                                current_inv.macro_name == macro_def.name
                            } else {
                                false
                            };

                            tracing::debug!(target: "codegen", "Continuing to parent macro: {}, exclude_current={}", bubbled_macro_def.name, should_exclude_current_frame);

                            // Update loop variables for next iteration instead of recursing
                            arg_parent_macro_name = ac_parent_macro_name.clone();
                            arg_name = ac.clone();
                            macro_def = bubbled_macro_def;

                            // If we should exclude current frame, reduce search depth
                            if should_exclude_current_frame {
                                search_depth = search_depth.saturating_sub(1);
                            }

                            // Continue the loop to process with updated variables
                            continue;
                        }
                        MacroArg::Ident(iden) => {
                            tracing::warn!(target: "codegen", "Found MacroArg::Ident IN \"{}\" Macro Invocation: \"{}\"! (arg_name={}, target_macro={}, search_depth={})", target_macro_invoc.1.macro_name, iden, arg_name, target_macro.name, search_depth);

                            // Check for a constant first (clone to release the lock)
                            let const_value_opt = {
                                let constants = contract
                                    .constants
                                    .lock()
                                    .map_err(|_| CodegenError::new(CodegenErrorKind::LockingError, AstSpan(vec![]), None))?;
                                constants.iter().find(|const_def| const_def.name.eq(iden)).map(|c| c.value.clone())
                            };

                            if let Some(const_value) = const_value_opt {
                                tracing::info!(target: "codegen", "ARGCALL IS CONSTANT: {:?}", const_value);
                                let push_bytes = match &const_value {
                                    ConstVal::Bytes(bytes) => {
                                        let hex_literal: String = bytes.as_str().to_string();
                                        format!("{:02x}{hex_literal}", 95 + hex_literal.len() / 2)
                                    }
                                    ConstVal::StoragePointer(sp) => {
                                        let hex_literal: String = bytes32_to_hex_string(sp, false);
                                        format!("{:02x}{hex_literal}", 95 + hex_literal.len() / 2)
                                    }
                                    ConstVal::BuiltinFunctionCall(bf) => {
                                        let push_value = Codegen::gen_builtin_bytecode(contract, bf, target_macro_invoc.1.span.clone())?;
                                        // For __RIGHTPAD, always use PUSH32 with full 32 bytes (no optimization)
                                        match bf.kind {
                                            BuiltinFunctionKind::RightPad => push_value.to_hex_full_with_opcode(),
                                            _ => push_value.to_hex_with_opcode(evm_version),
                                        }
                                    }
                                    ConstVal::Expression(expr) => {
                                        tracing::info!(target: "codegen", "EVALUATING CONSTANT EXPRESSION FOR \"{iden}\"");
                                        let evaluated = contract.evaluate_constant_expression(expr)?;
                                        literal_gen(evm_version, &evaluated)
                                    }
                                    ConstVal::Noop => {
                                        tracing::info!(target: "codegen", "CONSTANT \"{iden}\" IS __NOOP - GENERATING NO BYTECODE IN ARGCALL");
                                        String::new() // Generate no bytecode
                                    }
                                    ConstVal::FreeStoragePointer(fsp) => {
                                        // If this is reached in codegen stage,
                                        // `derive_storage_pointers`
                                        // method was not called on the AST.
                                        tracing::error!(target: "codegen", "STORAGE POINTERS INCORRECTLY DERIVED FOR \"{fsp:?}\"");
                                        return Err(CodegenError {
                                            kind: CodegenErrorKind::StoragePointersNotDerived,
                                            span: AstSpan(vec![]).boxed(),
                                            token: None,
                                        });
                                    }
                                };
                                *offset += push_bytes.len() / 2;
                                tracing::info!(target: "codegen", "OFFSET: {}, PUSH BYTES: {:?}", offset, push_bytes);
                                bytes.push_with_offset(starting_offset, Bytes::Raw(push_bytes));
                            } else if let Ok(o) = Opcode::from_str(iden) {
                                tracing::debug!(target: "codegen", "Found Opcode: {}", o);
                                let b = Bytes::Raw(o.to_string());
                                *offset += b.len();
                                bytes.push_with_offset(starting_offset, b);
                            } else {
                                tracing::debug!(target: "codegen", "Found Label Call: {}", iden);

                                // This should be equivalent to a label call.
                                bytes.push_with_offset(
                                    *offset,
                                    Bytes::JumpPlaceholder(JumpPlaceholderData::new(iden.to_owned(), PushOpcode::Push2, String::new())),
                                );

                                let scope_id = scope_mgr.parent_scope();
                                jump_table.insert(
                                    *offset,
                                    vec![Jump {
                                        label: iden.to_owned(),
                                        bytecode_index: 0,
                                        span: target_macro_invoc.1.span.clone(),
                                        scope_id,
                                    }],
                                );
                                *offset += 3;
                            }
                        }
                        MacroArg::ArgCallMacroInvocation(arg_name, invoc_args) => {
                            // Resolve the argument to get the actual macro name
                            // Use search_depth which may be reduced to exclude problematic frames
                            match resolve_argument_through_scope(arg_name, scope_mgr, search_depth, contract) {
                                Ok(actual_macro_name) => {
                                    // Now invoke the resolved macro with the provided arguments
                                    if let Some(called_macro) = contract.find_macro_by_name(&actual_macro_name) {
                                        let inner_mi = MacroInvocation {
                                            macro_name: actual_macro_name.clone(),
                                            args: invoc_args.clone(),
                                            span: span.clone(),
                                        };

                                        scope_mgr.push_macro_with_invocation(called_macro, *offset, inner_mi);
                                        let result = Codegen::macro_to_bytecode(
                                            evm_version,
                                            called_macro,
                                            contract,
                                            scope_mgr,
                                            *offset,
                                            false,
                                            None,
                                            relax_jumps,
                                        );
                                        // Note: macro_to_bytecode already pops the macro from scope_mgr

                                        match result {
                                            Ok(expanded_macro) => {
                                                let byte_len: usize = expanded_macro.bytes.iter().map(|seg| seg.bytes.len()).sum();
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
                                            kind: CodegenErrorKind::MissingMacroDefinition(actual_macro_name),
                                            span: span.clone_box(),
                                            token: None,
                                        });
                                    }
                                }
                                Err(e) => {
                                    return Err(CodegenError { kind: e, span: span.clone_box(), token: None });
                                }
                            }
                        }
                        MacroArg::MacroCall(inner_mi) => {
                            // Check if this macro is already on the scope stack AND we're in bubbling mode
                            // to prevent infinite recursion. Bubbling mode is when search_depth < full depth.
                            let is_already_in_scope = scope_mgr.macro_stack().iter().any(|m| m.name == inner_mi.macro_name);
                            let is_bubbling = search_depth < scope_mgr.depth();

                            if is_already_in_scope && is_bubbling {
                                tracing::warn!(target: "codegen", "Macro {} is already in scope stack while bubbling, skipping to prevent infinite recursion", inner_mi.macro_name);
                                // Don't expand - this macro is already being executed higher up in the stack
                                // and we're bubbling to resolve an ArgCall, which would cause infinite recursion
                                // This prevents M2(M3(<arg>)) where M3 tries to resolve <arg> from infinitely recursing
                            } else if !inner_mi.args.is_empty() {
                                // This is an inline expansion case (macro with arguments)
                                tracing::debug!(target: "codegen", "Processing inline MacroCall argument: {}", inner_mi.macro_name);

                                if let Some(called_macro) = contract.find_macro_by_name(&inner_mi.macro_name) {
                                    scope_mgr.push_macro_with_invocation(called_macro, *offset, inner_mi.clone());
                                    let result = Codegen::macro_to_bytecode(
                                        evm_version,
                                        called_macro,
                                        contract,
                                        scope_mgr,
                                        *offset,
                                        false,
                                        None,
                                        relax_jumps,
                                    );
                                    // Note: macro_to_bytecode already pops the macro from scope_mgr
                                    match result {
                                        Ok(expanded_macro) => {
                                            let byte_len: usize = expanded_macro.bytes.iter().map(|seg| seg.bytes.len()).sum();
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
                                        span: inner_mi.span.clone_box(),
                                        token: None,
                                    });
                                }
                            } else {
                                // This is a parameter substitution case (macro without arguments)
                                // Process it normally for substitution where <arg> appears
                                tracing::debug!(target: "codegen", "Processing parameter substitution MacroCall: {}", inner_mi.macro_name);

                                if let Some(called_macro) = contract.find_macro_by_name(&inner_mi.macro_name) {
                                    tracing::debug!(target: "codegen", "Found valid macro: {}", called_macro.name);

                                    scope_mgr.push_macro_with_invocation(called_macro, *offset, inner_mi.clone());
                                    let result = Codegen::macro_to_bytecode(
                                        evm_version,
                                        called_macro,
                                        contract,
                                        scope_mgr,
                                        *offset,
                                        false,
                                        None,
                                        relax_jumps,
                                    );
                                    // Note: macro_to_bytecode already pops the macro from scope_mgr
                                    match result {
                                        Ok(expanded_macro) => {
                                            let byte_len: usize = expanded_macro.bytes.iter().map(|seg| seg.bytes.len()).sum();
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
                                        span: inner_mi.span.clone_box(),
                                        token: None,
                                    });
                                }
                            }
                        }
                    }

                    // Successfully handled the argument, return
                    return Ok(());
                } else {
                    tracing::warn!(target: "codegen", "\"{}\" FOUND IN MACRO DEF \"{}\" BUT NOT IN MACRO INVOCATION \"{}\"!", arg_name, target_macro.name, target_macro_invoc.1.macro_name);
                }
            } else {
                // Argument not found in current macro parameters.
                // Before bubbling up, verify that the argument was actually passed to this macro.
                tracing::debug!(target: "codegen", "Argument \"{}\" not found in macro \"{}\", checking if it was passed to this macro", arg_name, macro_def.name);

                // Check if we can bubble - only if we're in a nested macro (depth > 1)
                // In the test case: MAIN (depth 0) -> M1 (depth 1) -> M2 (depth 2)
                // When M2 tries to access <arg>, we should NOT bubble because M2() was called without args
                // The key is: we only bubble if the current macro itself has parameters that might reference parent args

                // However, the current logic has an issue: when we recurse, we get the same `mis` from scope_mgr
                // So we need a different approach: check if the macro definition has this parameter
                // If not in current macro AND we're looking for an arg that wasn't passed to this macro, error out

                // The parent_scope_id.depth should point to a valid parent
                let parent_scope_id = scope_mgr.parent_scope();

                // Check: do we have a parent macro AND is that parent's invocation still in our mis?
                // The key insight: if this macro (macro_def) has NO parameters, it shouldn't access any args
                if macro_def.parameters.is_empty() {
                    // This macro has no parameters, so it can't legally access any argument
                    tracing::debug!(target: "codegen", "Macro \"{}\" has no parameters, cannot access argument \"{}\"", macro_def.name, arg_name);
                    return Err(CodegenError {
                        kind: CodegenErrorKind::MissingArgumentDefinition(arg_name.to_string()),
                        span: span.clone_box(),
                        token: None,
                    });
                }

                // This macro has parameters, but the argument wasn't found
                // Check if we can bubble to parent
                if search_depth > 1 {
                    // Check if parent exists
                    let parent_exists = scope_mgr.macro_at_depth(parent_scope_id.depth).is_some();
                    if parent_exists {
                        // Get the parent macro
                        if let Some(bubbled_macro) = scope_mgr.macro_at_depth(parent_scope_id.depth) {
                            tracing::debug!(target: "codegen", "Argument \"{}\" not in macro \"{}\" params, bubbling up to macro \"{}\"", arg_name, macro_def.name, bubbled_macro.name);

                            // Always reduce search depth when bubbling up (replicates old code behavior)
                            search_depth = search_depth.saturating_sub(1);
                            macro_def = bubbled_macro;

                            // Continue the loop to process with updated variables
                            continue;
                        }
                    }
                }

                // No parent scope or at the top level - this is truly an undefined argument
                return Err(CodegenError {
                    kind: CodegenErrorKind::MissingArgumentDefinition(arg_name.to_string()),
                    span: span.clone_box(),
                    token: None,
                });
            }
        } else {
            // This is a label call
            tracing::info!(target: "codegen", "RECURSE_BYTECODE ARG CALL DEFAULTING TO LABEL CALL: \"{}\"", arg_name);

            // Get the current invocation's span and offset
            let (invoc_offset, new_span) = scope_mgr
                .current_invocation_with_offset()
                .map(|(off, invoc_opt)| (off, invoc_opt.map(|inv| inv.span.clone()).unwrap_or_else(|| AstSpan(vec![]))))
                .unwrap_or((0, AstSpan(vec![])));

            let scope_id = scope_mgr.parent_scope();
            jump_table.insert(invoc_offset, vec![Jump { label: arg_name.to_owned(), bytecode_index: 0, span: new_span, scope_id }]);
            bytes.push_with_offset(
                *offset,
                Bytes::JumpPlaceholder(JumpPlaceholderData::new(arg_name.to_owned(), PushOpcode::Push2, String::new())),
            );
            *offset += 3;
        }

        // Successfully processed the argument, exit the loop
        return Ok(());
    } // end of loop
}
