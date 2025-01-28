use huff_neo_utils::prelude::*;

use crate::irgen::builtin_function;
use crate::Codegen;

/// Generates the respective Bytecode for a given Statement
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
) -> Result<Vec<(usize, Bytes)>, CodegenError> {
    let mut bytes = vec![];

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
                    span: mi.span.clone(),
                    token: None,
                });
            };

            tracing::info!(target: "codegen", "FOUND INNER MACRO: {}", ir_macro.name);

            // Tests may not be invoked
            if ir_macro.test {
                tracing::error!(target: "codegen", "Tests may not be invoked: {}", ir_macro.name);
                return Err(CodegenError {
                    kind: CodegenErrorKind::TestInvocation(ir_macro.name.clone()),
                    span: ir_macro.span.clone(),
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
                jump_table.insert(
                    *offset + stack_swaps.len() + 3, // PUSH2 + 2 bytes + stack_swaps.len()
                    vec![Jump { label: format!("goto_{}", &ir_macro.name), bytecode_index: 0, span: s.span.clone() }],
                );

                // Store return JUMPDEST PC on the stack and re-order the stack so that
                // the return JUMPDEST PC is below the function's stack inputs
                bytes.push((*offset, Bytes(format!("{}{:04x}{}", Opcode::Push2, *offset + stack_swaps.len() + 7, stack_swaps.join("")))));
                // Insert jump to outlined macro + jumpdest to return to
                bytes.push((
                    *offset + stack_swaps.len() + 3, // PUSH2 + 2 bytes + stack_swaps.len()
                    Bytes(format!("{}xxxx{}{}", Opcode::Push2, Opcode::Jump, Opcode::Jumpdest)),
                ));
                // PUSH2 + 2 bytes + stack_swaps.len() + PUSH2 + 2 bytes + JUMP + JUMPDEST
                *offset += stack_swaps.len() + 8;
            } else {
                // Recurse into macro invocation
                scope.push(ir_macro);
                mis.push((*offset, mi.clone()));

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
                table_instances.extend(res.table_instances);
                label_indices.extend(res.label_indices);
                utilized_tables.extend(res.utilized_tables);

                // Increase offset by byte length of recursed macro
                *offset += res.bytes.iter().map(|(_, b)| b.0.len()).sum::<usize>() / 2;
                // Add the macro's bytecode to the final result
                bytes = [bytes, res.bytes].concat()
            }
        }
        StatementType::Label(label) => {
            // Add JUMPDEST opcode to final result and add to label_indices
            tracing::info!(target: "codegen", "RECURSE BYTECODE GOT LABEL: {:?}", label.name);
            label_indices.insert(label.name.clone(), *offset);
            bytes.push((*offset, Bytes(Opcode::Jumpdest.to_string())));
            *offset += 1;
        }
        StatementType::LabelCall(label) => {
            // Generate code for a `LabelCall`
            // PUSH2 + 2 byte destination (placeholder for now, filled in `Codegen::fill_unmatched`
            tracing::info!(target: "codegen", "RECURSE BYTECODE GOT LABEL CALL: {}", label);
            jump_table.insert(*offset, vec![Jump { label: label.to_string(), bytecode_index: 0, span: s.span.clone() }]);
            bytes.push((*offset, Bytes(format!("{}xxxx", Opcode::Push2))));
            *offset += 3;
        }
        StatementType::BuiltinFunctionCall(bf) => {
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
        }
        sty => {
            tracing::error!(target: "codegen", "CURRENT MACRO DEF: {}", macro_def.name);
            tracing::error!(target: "codegen", "UNEXPECTED STATEMENT: {:?}", sty);
            return Err(CodegenError { kind: CodegenErrorKind::InvalidMacroStatement, span: s.span.clone(), token: None });
        }
    }

    Ok(bytes)
}
