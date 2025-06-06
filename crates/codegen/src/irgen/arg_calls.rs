use crate::Codegen;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::prelude::*;
use std::str::FromStr;
// Arguments can be literals, labels, opcodes, constants, or macro calls
// !! IF THERE IS AMBIGUOUS NOMENCLATURE
// !! (E.G. BOTH OPCODE AND LABEL ARE THE SAME STRING)
// !! COMPILATION _WILL_ ERROR

/// Arg Call Bubbling
#[allow(clippy::too_many_arguments)]
pub fn bubble_arg_call(
    evm_version: &EVMVersion,
    arg_name: &str,
    bytes: &mut Vec<(usize, Bytes)>,
    macro_def: &MacroDefinition,
    contract: &Contract,
    scope: &mut [&MacroDefinition],
    offset: &mut usize,
    // mis: Parent macro invocations and their indices
    mis: &mut [(usize, MacroInvocation)],
    jump_table: &mut JumpTable,
    span: &AstSpan,
) -> Result<(), CodegenError> {
    let starting_offset = *offset;

    if let Some(macro_invoc) = mis.last() {
        // Literal, Ident & Arg Call Check
        // First get this arg_nam position in the macro definition params
        if let Some(pos) = macro_def.parameters.iter().position(|r| r.name.as_ref().is_some_and(|s| s.eq(arg_name))) {
            tracing::info!(target: "codegen", "GOT \"{}\" POS IN ARG LIST: {}", arg_name, pos);

            if let Some(arg) = macro_invoc.1.args.get(pos) {
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
                    MacroArg::ArgCall(ac, arg_span) => {
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
                                })
                            }
                        };
                        let mis_len = mis.len();
                        let ac_ = &ac.to_string();
                        return if last_mi.1.macro_name.eq(&macro_def.name) {
                            bubble_arg_call(
                                evm_version,
                                ac_,
                                bytes,
                                bubbled_macro_invocation,
                                contract,
                                new_scope,
                                offset,
                                &mut mis[..mis_len.saturating_sub(1)].to_vec(),
                                jump_table,
                                arg_span,
                            )
                        } else {
                            bubble_arg_call(
                                evm_version,
                                ac_,
                                bytes,
                                bubbled_macro_invocation,
                                contract,
                                new_scope,
                                offset,
                                mis,
                                jump_table,
                                span,
                            )
                        };
                    }
                    MacroArg::Ident(iden) => {
                        tracing::debug!(target: "codegen", "Found MacroArg::Ident IN \"{}\" Macro Invocation: \"{}\"!", macro_invoc.1.macro_name, iden);

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
                                    Codegen::gen_builtin_bytecode(evm_version, contract, bf, macro_invoc.1.span.clone())?
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
                            jump_table.insert(
                                *offset,
                                vec![Jump { label: iden.to_owned(), bytecode_index: 0, span: macro_invoc.1.span.clone() }],
                            );
                            *offset += 3;
                        }
                    }
                    MacroArg::MacroCall(inner_mi) => {
                        tracing::debug!(target: "codegen", "Found MacroArg::MacroCall IN \"{}\" Macro Invocation: \"{}\"!", macro_invoc.1.macro_name, inner_mi.macro_name);

                        if let Some(called_macro) = contract.find_macro_by_name(&inner_mi.macro_name) {
                            tracing::debug!(target: "codegen", "Found valid macro: {}", called_macro.name);
                            let mut new_scope = scope.to_vec();
                            new_scope.push(called_macro);
                            let mut new_mis = mis.to_vec();
                            new_mis.push((starting_offset, inner_mi.clone()));
                            match Codegen::macro_to_bytecode(
                                evm_version,
                                called_macro,
                                contract,
                                &mut new_scope,
                                starting_offset,
                                &mut new_mis,
                                false,
                                None,
                            ) {
                                Ok(expanded_macro) => {
                                    bytes.extend(expanded_macro.bytes);
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
            } else {
                tracing::warn!(target: "codegen", "\"{}\" FOUND IN MACRO DEF BUT NOT IN MACRO INVOCATION!", arg_name);
            }
        } else {
            return Err(CodegenError {
                kind: CodegenErrorKind::MissingArgumentDefinition(arg_name.to_string()),
                span: span.clone(),
                token: None,
            });
        }
    } else {
        // This is a label call
        tracing::info!(target: "codegen", "RECURSE_BYTECODE ARG CALL DEFAULTING TO LABEL CALL: \"{}\"", arg_name);
        let new_span = match mis.last() {
            Some(mi) => mi.1.span.clone(),
            None => AstSpan(vec![]),
        };
        jump_table.insert(
            mis.last().map(|mi| mi.0).unwrap_or_else(|| 0),
            vec![Jump { label: arg_name.to_owned(), bytecode_index: 0, span: new_span }],
        );
        bytes.push((*offset, Bytes(format!("{}xxxx", Opcode::Push2))));
        *offset += 3;
    }

    Ok(())
}
