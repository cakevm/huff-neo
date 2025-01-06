use crate::cheats::{HuffCheatCode, HUFF_CHEATS_MAP};
use alloy_primitives::{hex, Address, Log};
use lazy_static::lazy_static;
use revm::{
    interpreter::{CallInputs, CallOutcome, CreateInputs, CreateOutcome, InstructionResult, Interpreter, InterpreterResult},
    Database, EvmContext, Inspector,
};
use std::str::FromStr;

lazy_static! {
    pub static ref CHEATS_ADDR: Address = Address::from_str("00000000000000000000000000000000bEefbabe").unwrap();
}

#[derive(Debug, Default)]
pub struct CheatsInspector {
    pub logs: Vec<(u32, String)>,
}

impl<DB> Inspector<DB> for CheatsInspector
where
    DB: Database,
{
    fn log(&mut self, _interp: &mut Interpreter, _context: &mut EvmContext<DB>, _log: &Log) {
        unimplemented!()
    }

    fn call(&mut self, _context: &mut EvmContext<DB>, _inputs: &mut CallInputs) -> Option<CallOutcome> {
        None
    }

    fn call_end(&mut self, _context: &mut EvmContext<DB>, inputs: &CallInputs, outcome: CallOutcome) -> CallOutcome {
        if inputs.caller == *CHEATS_ADDR && inputs.input.len() >= 64 {
            // All cheatcodes calls must include the cheatcode key and the current pc in the first
            // 64 bytes of calldata.
            fn bytes_to_u32(b: &[u8]) -> u32 {
                u32::from_str_radix(hex::encode(b).as_str(), 16).unwrap_or(0)
            }
            let cheat_key = bytes_to_u32(&inputs.input[0..32]);
            let pc = bytes_to_u32(&inputs.input[32..64]);

            if let Some(HuffCheatCode::Log) = HUFF_CHEATS_MAP.get(&cheat_key) {
                // In Huffmate, the LOG macro sends 96 bytes of calldata to our cheatcode
                // address, laid out as follows:
                // ╔════════╦═══════════════╗
                // ║ Offset ║     Value     ║
                // ╠════════╬═══════════════╣
                // ║ 0x00   ║ cheat_key     ║
                // ║ 0x20   ║ pc            ║
                // ║ 0x40   ║ log_item      ║
                // ╚════════╩═══════════════╝
                //
                // #define macro LOG() = takes (1) {
                //     // Input stack:   [log_item]
                //     pc             // [pc, log_item]
                //     0x01           // [log_cheatcode, pc, log_item]
                //     0x00 mstore    // [pc, log_item]
                //     0x20 mstore    // [log_item]
                //     0x40 mstore    // []
                //     0x00 dup1      // [0x00, 0x00]
                //     0x60 dup2      // [0x00, 0x60, 0x00, 0x00]
                //     0x00000000000000000000000000000000bEefbabe
                //     gas            // [gas, beef_babe, 0x00, 0x60, 0x00, 0x00]
                //     staticcall pop // []
                // }

                // Check if we have exactly one 32 byte input
                if inputs.input.len() != 96 {
                    return CallOutcome::new(
                        InterpreterResult::new(InstructionResult::Revert, outcome.output().clone(), outcome.gas()),
                        outcome.memory_offset,
                    );
                }

                let log_item = hex::encode(&inputs.input[64..96]);
                self.logs.push((pc, log_item));
            }
        }

        outcome
    }

    /*
    // TODO: Have test for this required
    fn create(
        &mut self,
        _context: &mut EvmContext<DB>,
        inputs: &mut CreateInputs,
    ) -> (InstructionResult, Option<Address>, Gas, Bytes) {
        (InstructionResult::Continue, None, Gas::new(inputs.gas_limit), Bytes::new())
    }
     */

    fn create_end(&mut self, _context: &mut EvmContext<DB>, _inputs: &CreateInputs, outcome: CreateOutcome) -> CreateOutcome {
        outcome
    }
}
