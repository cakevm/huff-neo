use huff_neo_codegen::Codegen;
use huff_neo_lexer::*;
use huff_neo_parser::*;
use huff_neo_utils::prelude::*;

/// Test that jump relaxation reduces bytecode size for small contracts
#[test]
fn test_jump_relaxation_small_contract() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            start:
                0x01
                dup1
                end jump
            end:
                0x02
                0x03
                add
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    assert_eq!(bytecode_without.len() - bytecode_with.len(), 2, "Relaxation should reduce bytecode by 1 byte");
}

/// Test forward jumps
#[test]
fn test_jump_relaxation_forward_jump() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            forward jump
            0x01
            0x02
            forward:
                0x00
                mstore
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // With relaxation, should use PUSH1 (60) instead of PUSH2 (61)
    assert!(bytecode_with.len() < bytecode_without.len(), "Forward jump should be optimized");
    // After relaxation, JUMPDEST shifts from byte 8 to byte 7, so jump target should be 0x07
    assert_eq!(bytecode_with, "600756600160025b5f52", "Should use PUSH1 for near jump and point to correct offset");
}

/// Test backward jumps
#[test]
fn test_jump_relaxation_backward_jump() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            loop:
                0x01
                0x01
                add
                dup1
                loop jump
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Backward jump should be optimized to PUSH1
    assert!(bytecode_with.len() < bytecode_without.len(), "Backward jump should be optimized");
    assert_eq!(bytecode_with, "5b600160010180600056", "Should use PUSH1 for backward jump");
}

/// Test multiple jumps
#[test]
fn test_jump_relaxation_multiple_jumps() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            label1 jump
            label1:
                0x01
                label2 jump
            label2:
                0x02
                label3 jump
            label3:
                0x03
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // All three jumps should be optimized
    assert!(bytecode_with.len() < bytecode_without.len(), "Multiple jumps should be optimized");
    // After relaxation, all JUMPDESTs shift backward, so jump targets update accordingly
    assert_eq!(bytecode_with, "6003565b60016009565b6002600f565b6003", "All jumps should use PUSH1 with correct offsets");
}

/// Test for loop creating large gap - jump should NOT be optimized
#[test]
fn test_jump_relaxation_large_gap_no_optimization() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            end jump
            for(i in 0..50) {
                0x01
                0x02
                add
                pop
            }
            end:
                0xff
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Large gap means jump target > 255, so PUSH2 must be preserved
    assert_eq!(bytecode_without.len(), bytecode_with.len(), "Jump over large gap should NOT be optimized");
    // Verify bytecode contains PUSH2 (0x61) at the start for the jump
    assert!(bytecode_with.starts_with("61"), "Should still use PUSH2 for far jump");
}

/// Test cross jumps - mixed optimization with PUSH1 and PUSH2
#[test]
fn test_jump_relaxation_cross_jumps_mixed() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            near jump
            0x01 0x01 0x01
            near:
                0x00
                far jump
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
                0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a
            far:
                0xaa
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Near jump should be optimized to PUSH1, far jump should stay PUSH2
    assert!(bytecode_with.len() < bytecode_without.len(), "Near jump should be optimized even with far jump present");
}

/// Test multiple cross jumps with for loops
#[test]
fn test_jump_relaxation_multiple_cross_jumps() {
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            label1 jump
            for(i in 0..5) {
                0x02
            }
            label1:
                0x01
                label2 jump
            for(i in 0..5) {
                0x03
            }
            label2:
                0x04
                label3 jump
            for(i in 0..5) {
                0x05
            }
            label3:
                0x06
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // All jumps should still be optimizable with small for loops
    assert!(bytecode_with.len() < bytecode_without.len(), "Jumps with small for loop gaps should be optimized");
}

#[test]
fn test_jump_relaxation_correct_label_positions() {
    // This test verifies that JUMPDEST positions are calculated correctly and jump
    // placeholders resolve to the exact JUMPDEST position (not off by one).
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            start jump
            0x01
            start:
                push0
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // Expected bytecode layout:
    // Byte 0: 0x60 (PUSH1 for jump)
    // Byte 1: 0x05 (jump target - should point to JUMPDEST)
    // Byte 2: 0x56 (JUMP)
    // Byte 3: 0x60 (PUSH1)
    // Byte 4: 0x01 (value)
    // Byte 5: 0x5b (JUMPDEST) <-- "start" label is here
    // Byte 6: 0x5f (PUSH0)

    // Hex string: 60 05 56 60 01 5b 5f
    assert_eq!(bytecode_with, "60055660015b5f", "Jump should resolve to JUMPDEST at byte 5 (0x05)");
}

/// Test that jump table entries are correctly updated after jump relaxation
/// This verifies that jump table label offsets are recalculated after PUSH2->PUSH1 optimization
#[test]
fn test_jump_relaxation_with_jump_table() {
    let source = r#"
        #define jumptable__packed DISPATCH_TABLE {
            handler_0 handler_1 handler_2
        }

        #define macro CONSTRUCTOR() = takes(0) returns(0) {
            // Multiple short jumps that will be optimized to PUSH1
            start jump
            0x01
            start:
                next jump
            0x02
            next:
                table_init jump
            0x03

            // Initialize jump table
            table_init:
                __tablesize(DISPATCH_TABLE) __tablestart(DISPATCH_TABLE) 0x00 codecopy

            // Load selector and dispatch
            0x00 calldataload
            0x02 byte
            0x02 mul
            __tablestart(DISPATCH_TABLE) add
            mload
            jump

            // Handlers - labels referenced in the jump table
            handler_0:
                0x0A
                0x00
                mstore
                stop
            handler_1:
                0x0B
                0x00
                mstore
                stop
            handler_2:
                0x0C
                0x00
                mstore
                stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let (bytecode_with, _) = Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();
    let (bytecode_without, _) = Codegen::generate_constructor_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();

    // With relaxation, bytecode should be smaller
    assert!(bytecode_with.len() < bytecode_without.len(), "Relaxation should reduce bytecode size");

    // Jump table is at the end: 3 entries × 2 bytes each = 6 bytes = 12 hex chars
    let table_hex = &bytecode_with[bytecode_with.len() - 12..];

    // Parse the three 2-byte offsets
    let handler_0_offset = usize::from_str_radix(&table_hex[0..4], 16).unwrap();
    let handler_1_offset = usize::from_str_radix(&table_hex[4..8], 16).unwrap();
    let handler_2_offset = usize::from_str_radix(&table_hex[8..12], 16).unwrap();

    // Verify offsets are within the code portion (before the table)
    let code_and_table_size = bytecode_with.len() / 2;
    let table_start = code_and_table_size - 6;

    assert!(handler_0_offset < table_start, "Handler 0 offset should point to code, not table");
    assert!(handler_1_offset < table_start, "Handler 1 offset should point to code, not table");
    assert!(handler_2_offset < table_start, "Handler 2 offset should point to code, not table");

    // Verify the offsets point to JUMPDEST opcodes (0x5b)
    // This is the critical test: without the fix, these would point past the JUMPDEST
    let handler_0_opcode = &bytecode_with[handler_0_offset * 2..handler_0_offset * 2 + 2];
    let handler_1_opcode = &bytecode_with[handler_1_offset * 2..handler_1_offset * 2 + 2];
    let handler_2_opcode = &bytecode_with[handler_2_offset * 2..handler_2_offset * 2 + 2];

    assert_eq!(handler_0_opcode, "5b", "Handler 0 in jump table should point to JUMPDEST (0x5b), got {}", handler_0_opcode);
    assert_eq!(handler_1_opcode, "5b", "Handler 1 in jump table should point to JUMPDEST (0x5b), got {}", handler_1_opcode);
    assert_eq!(handler_2_opcode, "5b", "Handler 2 in jump table should point to JUMPDEST (0x5b), got {}", handler_2_opcode);
}

/// Test jump relaxation at the exact PUSH1/PUSH2 boundary (255 bytes)
/// This tests the edge case where a jump target at exactly byte 255 should use PUSH1 0xFF
#[test]
fn test_jump_relaxation_boundary_255() {
    // Goal: Position a JUMPDEST at exactly byte 255 (0xFF) after relaxation
    // This is the maximum offset that can be represented with PUSH1
    //
    // Strategy: Start with JUMPDEST beyond 255 so it uses PUSH2, then let relaxation shrink it to 255
    //
    // Without relaxation: PUSH2 is used because initial target is at byte 256+
    // With relaxation: The jump itself shrinks from PUSH2 (4 bytes) to PUSH1 (3 bytes),
    //                  causing the target to shift from 256 to 255
    let source = r#"
        #define macro MAIN() = takes(0) returns(0) {
            // Single jump that will be relaxed, causing the target label to shift
            target jump

            // Use STOP opcodes (0x00, 1 byte each) for precise padding
            //
            // Without relaxation:
            //   - "target jump" = PUSH2 + 2 bytes + JUMP = 4 bytes
            //   - Padding: 252 STOPs (bytes 4-255)
            //   - target JUMPDEST at byte 256
            //   - final STOP at byte 257
            //   - Total bytecode: 258 bytes
            //
            // With relaxation:
            //   - "target jump" shrinks to PUSH1 + 1 byte + JUMP = 3 bytes
            //   - Padding: 252 STOPs (bytes 3-254)
            //   - target JUMPDEST at byte 255
            //   - final STOP at byte 256
            //   - Total bytecode: 257 bytes

            // 252 STOP opcodes for padding
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop stop
            stop stop stop stop stop stop stop stop stop stop stop stop

            target:
                stop
        }
    "#;

    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let lexer = Lexer::new(flattened_source);
    let tokens = lexer.into_iter().map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut parser = Parser::new(tokens, None);
    let mut contract = parser.parse().unwrap();
    contract.derive_storage_pointers();

    let bytecode_without = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, false).unwrap();
    let bytecode_with = Codegen::generate_main_bytecode(&EVMVersion::default(), &contract, None, true).unwrap();

    // With relaxation, should be smaller
    assert!(bytecode_with.len() < bytecode_without.len(), "Relaxation should reduce bytecode size");

    // After relaxation, JUMPDEST shifts from byte 256 to byte 255, allowing PUSH1 0xFF
    // The bytecode should start with PUSH1 0xFF JUMP then STOPs, ending with JUMPDEST
    assert!(bytecode_with.starts_with("60ff56"), "Should use PUSH1 0xFF for jump to byte 255");
    assert!(bytecode_with.ends_with("5b00"), "Should end with JUMPDEST STOP");
}
