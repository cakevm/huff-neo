use alloy_primitives::{Address, U256};
use foundry_evm::traces::{CallTraceStep, DecodedInternalCall, DecodedTraceStep, SparsedTraceArena};
use revm::bytecode::opcode::{JUMP, JUMPDEST};
use serde::Serialize;

/// An outlined Huff function (`#define fn`) of a compiled test, which traces show as an internal call
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HuffFunction {
    /// The name of the function
    pub name: String,
    /// The name of the file that defines the function, which traces show before the name
    pub file: Option<String>,
    /// The program counter of the JUMPDEST the function starts at
    pub entry_pc: usize,
    /// How many stack items the function takes
    pub takes: usize,
    /// How many stack items the function returns
    pub returns: usize,
}

/// A call into a Huff function that has not returned yet
struct OpenCall {
    /// Index of the function in the list of functions
    function: usize,
    /// Index of the step that jumps into the function
    start: usize,
    /// Program counter of that jump; the function returns to the JUMPDEST right after it
    jump_pc: usize,
}

/// Marks the calls of Huff functions in the traces of the test at `address` as internal calls, so
/// the trace shows each call with its arguments and return values.
///
/// It needs the jump steps that `--decode-internal` records. A call jumps to the function's entry
/// JUMPDEST and returns to the JUMPDEST right after the calling jump; a call that never returns, for
/// example because it reverts, is left out.
pub fn decode_huff_internal_calls(arena: &mut SparsedTraceArena, address: Address, functions: &[HuffFunction]) {
    if functions.is_empty() {
        return;
    }

    for node in arena.arena.nodes_mut().iter_mut().filter(|node| node.trace.address == address && !node.trace.kind.is_any_create()) {
        let steps = &mut node.trace.steps;
        let mut open: Vec<OpenCall> = Vec::new();

        for i in 1..steps.len() {
            if steps[i].op.get() != JUMPDEST || steps[i - 1].op.get() != JUMP {
                continue;
            }
            let pc = steps[i].pc;

            // A jump back to the JUMPDEST after a calling jump returns from that call
            if let Some(depth) = open.iter().rposition(|call| call.jump_pc + 1 == pc) {
                let call = &open[depth];
                let function = &functions[call.function];
                let decoded = DecodedInternalCall {
                    func_name: match &function.file {
                        Some(file) => format!("{file}::{}", function.name),
                        None => function.name.clone(),
                    },
                    args: Some(top_of_stack(&steps[call.start + 1], function.takes)),
                    return_data: (function.returns > 0).then(|| top_of_stack(&steps[i], function.returns)),
                };
                steps[call.start].decoded = Some(Box::new(DecodedTraceStep::InternalCall(decoded, i)));
                open.truncate(depth);
                continue;
            }

            if let Some(function) = functions.iter().position(|function| function.entry_pc == pc) {
                open.push(OpenCall { function, start: i - 1, jump_pc: steps[i - 1].pc });
            }
        }
    }
}

/// The top `count` stack items of a step, in the order they were pushed
fn top_of_stack(step: &CallTraceStep, count: usize) -> Vec<String> {
    let stack: &[U256] = step.stack.as_deref().unwrap_or_default();
    stack[stack.len().saturating_sub(count)..].iter().map(|word| format!("{word:#x}")).collect()
}
