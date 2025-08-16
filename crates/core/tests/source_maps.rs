use std::sync::Arc;

use huff_neo_core::Compiler;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::prelude::*;

#[test]
fn test_exact_source_positions_simple() {
    let source = r#"#define macro MAIN() = takes(0) returns (0) {
    0x42
    0x00
    sstore
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();

            assert_eq!(runtime_map.len(), 3);

            // First entry: "0x42" -> PUSH1 0x42 (2 bytes at PC=0, IC=0)
            assert_eq!(runtime_map[0].ic, 0);
            assert_eq!(runtime_map[0].pc, 0);
            assert_eq!(runtime_map[0].bytecode_length, 2); // PUSH1 0x42 = 2 bytes
            assert_eq!(runtime_map[0].source_length, 4);
            assert_eq!(runtime_map[0].source_start, 50);
            assert_eq!(&source[50..54], "0x42");

            // Second entry: "0x00" -> PUSH0 (1 byte at PC=2, IC=1)
            assert_eq!(runtime_map[1].ic, 1);
            assert_eq!(runtime_map[1].pc, 2);
            assert_eq!(runtime_map[1].bytecode_length, 1); // PUSH0 = 1 byte
            assert_eq!(runtime_map[1].source_length, 4);
            assert_eq!(runtime_map[1].source_start, 59);
            assert_eq!(&source[59..63], "0x00");

            // Third entry: "sstore" -> SSTORE (1 byte at PC=3, IC=2)
            assert_eq!(runtime_map[2].ic, 2);
            assert_eq!(runtime_map[2].pc, 3);
            assert_eq!(runtime_map[2].bytecode_length, 1); // SSTORE = 1 byte
            assert_eq!(runtime_map[2].source_length, 6);
            assert_eq!(runtime_map[2].source_start, 68);
            assert_eq!(&source[68..74], "sstore");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_exact_positions_with_constructor() {
    let source = r#"#define macro CONSTRUCTOR() = takes(0) returns (0) {
    0x01
    0x00
    sstore
}

#define macro MAIN() = takes(0) returns (0) {
    0x00
    sload
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            // Check constructor map
            assert!(artifact.constructor_map.is_some());
            let constructor_map = artifact.constructor_map.unwrap();
            assert_eq!(constructor_map.len(), 3);

            // Constructor first: "0x01" -> PUSH1 0x01 (2 bytes)
            assert_eq!(constructor_map[0].ic, 0);
            assert_eq!(constructor_map[0].pc, 0);
            assert_eq!(constructor_map[0].source_length, 4);
            assert_eq!(constructor_map[0].source_start, 57);
            assert_eq!(&source[57..61], "0x01");

            // Constructor second: "0x00" -> PUSH0 (1 byte)
            assert_eq!(constructor_map[1].ic, 1);
            assert_eq!(constructor_map[1].pc, 2);
            assert_eq!(constructor_map[1].source_length, 4);
            assert_eq!(constructor_map[1].source_start, 66);
            assert_eq!(&source[66..70], "0x00");

            // Constructor third: "sstore" -> SSTORE (1 byte)
            assert_eq!(constructor_map[2].ic, 2);
            assert_eq!(constructor_map[2].pc, 3);
            assert_eq!(constructor_map[2].source_length, 6);
            assert_eq!(constructor_map[2].source_start, 75);
            assert_eq!(&source[75..81], "sstore");

            // Check runtime map
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert_eq!(runtime_map.len(), 2);

            // Runtime first: "0x00" -> PUSH0 (1 byte)
            assert_eq!(runtime_map[0].ic, 0);
            assert_eq!(runtime_map[0].pc, 0);
            assert_eq!(runtime_map[0].source_length, 4);
            assert_eq!(runtime_map[0].source_start, 135);
            assert_eq!(&source[135..139], "0x00");

            // Runtime second: "sload" -> SLOAD (1 byte)
            assert_eq!(runtime_map[1].ic, 1);
            assert_eq!(runtime_map[1].pc, 1);
            assert_eq!(runtime_map[1].source_length, 5);
            assert_eq!(runtime_map[1].source_start, 144);
            assert_eq!(&source[144..149], "sload");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_exact_positions_nested_macro() {
    let source = r#"#define macro ADD() = takes(2) returns (1) {
    add
}

#define macro MAIN() = takes(0) returns (0) {
    0x01
    0x02
    ADD()
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert_eq!(runtime_map.len(), 3);

            // First: "0x01"
            assert_eq!(runtime_map[0].source_start, 106);
            assert_eq!(&source[106..110], "0x01");

            // Second: "0x02"
            assert_eq!(runtime_map[1].source_start, 115);
            assert_eq!(&source[115..119], "0x02");

            // Third: "add" from inside ADD() macro definition (not the invocation)
            assert_eq!(runtime_map[2].source_start, 49);
            assert_eq!(runtime_map[2].source_length, 3);
            assert_eq!(&source[49..52], "add");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_exact_positions_with_constant() {
    let source = r#"#define constant VAL = 0x42

#define macro MAIN() = takes(0) returns (0) {
    [VAL]
    0x00
    mstore
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert_eq!(runtime_map.len(), 3);

            // First: [VAL] constant reference -> PUSH1 0x42 (2 bytes)
            assert_eq!(runtime_map[0].source_start, 79);
            assert_eq!(runtime_map[0].source_length, 5);
            assert_eq!(&source[79..84], "[VAL]");

            // Second: "0x00" -> PUSH0 (1 byte)
            assert_eq!(runtime_map[1].source_start, 89);
            assert_eq!(runtime_map[1].source_length, 4);
            assert_eq!(&source[89..93], "0x00");

            // Third: "mstore" -> MSTORE (1 byte)
            assert_eq!(runtime_map[2].source_start, 98);
            assert_eq!(runtime_map[2].source_length, 6);
            assert_eq!(&source[98..104], "mstore");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_exact_positions_with_jumps() {
    let source = r#"#define macro MAIN() = takes(0) returns (0) {
    0x01
    skip jumpi
    0x02
    skip:
        0x03
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert!(runtime_map.len() >= 5);

            // First: "0x01"
            assert_eq!(runtime_map[0].source_start, 50);
            assert_eq!(&source[50..54], "0x01");

            // Jump label and jumpi
            assert_eq!(runtime_map[1].source_start, 59);
            assert_eq!(runtime_map[1].source_length, 4);
            assert_eq!(&source[59..63], "skip");

            // After jumpi
            assert_eq!(runtime_map[2].source_start, 64);
            assert_eq!(&source[64..69], "jumpi");

            // "0x02"
            assert_eq!(runtime_map[3].source_start, 74);
            assert_eq!(&source[74..78], "0x02");

            // Label "skip:" generates a JUMPDEST
            assert_eq!(runtime_map[4].source_start, 83);
            assert_eq!(runtime_map[4].source_length, 4);
            assert_eq!(&source[83..87], "skip");

            // "0x03"
            assert_eq!(runtime_map[5].source_start, 97);
            assert_eq!(&source[97..101], "0x03");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_builtin_functions_source_map() {
    let source = r#"#define function transfer(address,uint256) nonpayable returns ()
#define event Transfer(address,address,uint256)

#define macro EMIT_TRANSFER() = takes(3) returns (0) {
    __EVENT_HASH(Transfer)
    0x00 0x00
    log3
}

#define macro MAIN() = takes(0) returns (0) {
    0x00 calldataload 0xE0 shr
    
    __FUNC_SIG(transfer) eq transfer_fn jumpi
    
    0x00 0x00 revert
    
    transfer_fn:
        0x04 calldataload
        0x24 calldataload
        caller
        EMIT_TRANSFER()
        stop
}"#;

    let full_source =
        FileSource { source: Some(source.to_string()), path: "builtins.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["builtins.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert!(!runtime_map.is_empty());

            // Should have entries for builtin function expansions
            assert!(runtime_map.len() > 10);

            // Verify all entries have valid source positions
            for entry in &runtime_map {
                assert!(entry.source_length > 0, "Source length should be positive");
                assert!(entry.bytecode_length > 0, "Bytecode length should be positive");
            }
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_empty_constructor_source_map() {
    let source = r#"#define macro MAIN() = takes(0) returns (0) {
    0x42
    0x00
    sstore
    stop
}"#;

    let full_source =
        FileSource { source: Some(source.to_string()), path: "no_constructor.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler =
        Compiler::new(&evm_version, Arc::new(vec!["no_constructor.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            // No CONSTRUCTOR macro = no constructor map
            assert!(artifact.constructor_map.is_none() || artifact.constructor_map.as_ref().unwrap().is_empty());

            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert_eq!(runtime_map.len(), 4);

            // Verify each entry
            assert_eq!(runtime_map[0].source_start, 50);
            assert_eq!(&source[50..54], "0x42");

            assert_eq!(runtime_map[1].source_start, 59);
            assert_eq!(&source[59..63], "0x00");

            assert_eq!(runtime_map[2].source_start, 68);
            assert_eq!(&source[68..74], "sstore");

            assert_eq!(runtime_map[3].source_start, 79);
            assert_eq!(&source[79..83], "stop");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_source_map_with_flattened_content() {
    // Simulate a flattened file where imports have been resolved
    let flattened_content = r#"// From lib.huff
#define macro ADD_TWO() = takes(2) returns (1) {
    add
}

#define macro MUL_TWO() = takes(2) returns (1) {
    mul
}

// From main.huff
#define macro CONSTRUCTOR() = takes(0) returns (0) {
    0x42
    0x00
    sstore
}

#define macro MAIN() = takes(0) returns (0) {
    0x01
    0x02
    ADD_TWO()
    0x03
    MUL_TWO()
}"#;

    let source =
        FileSource { source: Some(flattened_content.to_string()), path: "main.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["main.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            // Check constructor map
            assert!(artifact.constructor_map.is_some());
            let constructor_map = artifact.constructor_map.unwrap();
            assert!(!constructor_map.is_empty());

            // Check runtime map
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert_eq!(runtime_map.len(), 5);

            // Verify that PC values are sequential based on instruction sizes
            // We can't verify exact sizes without knowing bytecode lengths,
            // but we can verify they're monotonically increasing
            let mut last_pc = 0;
            for (i, entry) in runtime_map.iter().enumerate() {
                if i > 0 {
                    assert!(entry.pc > last_pc, "PC should increase: {} <= {}", entry.pc, last_pc);
                }
                last_pc = entry.pc;
            }

            // Check that macro expansions point to definitions
            // "add" should be from ADD_TWO definition
            assert_eq!(runtime_map[2].source_start, 70);
            assert_eq!(&flattened_content[70..73], "add");

            // "mul" should be from MUL_TWO definition
            assert_eq!(runtime_map[4].source_start, 130);
            assert_eq!(&flattened_content[130..133], "mul");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_source_map_complex_nesting() {
    let complex_content = r#"#define macro INNER() = takes(0) returns (1) {
    0x42
}

#define macro MIDDLE() = takes(0) returns (1) {
    INNER()
    0x01
    add
}

#define macro OUTER() = takes(0) returns (1) {
    MIDDLE()
    0x02
    mul
}

#define macro MAIN() = takes(0) returns (0) {
    OUTER()
    0x00
    mstore
    0x20
    0x00
    return
}"#;

    let source =
        FileSource { source: Some(complex_content.to_string()), path: "complex.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["complex.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();
            assert!(runtime_map.len() >= 9);

            // Verify that PC values are sequential based on instruction sizes
            // We can't verify exact sizes without knowing bytecode lengths,
            // but we can verify they're monotonically increasing
            let mut last_pc = 0;
            for (i, entry) in runtime_map.iter().enumerate() {
                if i > 0 {
                    assert!(entry.pc > last_pc, "PC should increase: {} <= {}", entry.pc, last_pc);
                }
                last_pc = entry.pc;
            }

            // Check specific opcode positions from nested macros
            // "0x42" from INNER
            assert_eq!(runtime_map[0].source_start, 51);
            assert_eq!(&complex_content[51..55], "0x42");

            // "0x01" from MIDDLE
            assert_eq!(runtime_map[1].source_start, 123);
            assert_eq!(&complex_content[123..127], "0x01");

            // "add" from MIDDLE
            assert_eq!(runtime_map[2].source_start, 132);
            assert_eq!(&complex_content[132..135], "add");
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_source_map_points_to_actual_opcodes_not_invocations() {
    let source = r#"#define macro STORE_ONE() = takes(0) returns (0) {
    0x01
    0x00
    sstore
}

#define macro LOAD_ZERO() = takes(0) returns (1) {
    0x00
    sload
}

#define macro COMPLEX() = takes(0) returns (0) {
    STORE_ONE()
    LOAD_ZERO()
    pop
}

#define macro MAIN() = takes(0) returns (0) {
    COMPLEX()
    stop
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();

            // Verify exact positions map to actual opcodes, not invocations
            // "0x01" from STORE_ONE
            assert_eq!(runtime_map[0].source_start, 55);
            assert_eq!(&source[55..59], "0x01");

            // "0x00" from STORE_ONE
            assert_eq!(runtime_map[1].source_start, 64);
            assert_eq!(&source[64..68], "0x00");

            // "sstore" from STORE_ONE
            assert_eq!(runtime_map[2].source_start, 73);
            assert_eq!(&source[73..79], "sstore");

            // "0x00" from LOAD_ZERO
            assert_eq!(runtime_map[3].source_start, 138);
            assert_eq!(&source[138..142], "0x00");

            // "sload" from LOAD_ZERO
            assert_eq!(runtime_map[4].source_start, 147);
            assert_eq!(&source[147..152], "sload");

            // "pop" from COMPLEX
            assert_eq!(runtime_map[5].source_start, 241);
            assert_eq!(&source[241..244], "pop");

            // "stop" from MAIN
            assert_eq!(runtime_map[6].source_start, 312);
            assert_eq!(&source[312..316], "stop");

            // Verify we don't have entries pointing to macro invocations
            // STORE_ONE() is at 217-226, LOAD_ZERO() at 232-242, COMPLEX() at 277-285
            for entry in &runtime_map {
                assert!(!(entry.source_start == 217 && entry.source_length == 9)); // STORE_ONE()
                assert!(!(entry.source_start == 232 && entry.source_length == 10)); // LOAD_ZERO()
                assert!(!(entry.source_start == 277 && entry.source_length == 8)); // COMPLEX()
            }
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_source_map_with_macro_arguments() {
    let source = r#"#define macro ADD_CONST(const) = takes(1) returns (1) {
    <const>
    add
}

#define macro MUL_CONST(const) = takes(1) returns (1) {
    <const>
    mul
}

#define macro MAIN() = takes(0) returns (0) {
    0x05
    0x03
    ADD_CONST(0x02)
    MUL_CONST(0x04)
    0x00
    mstore
}"#;

    let full_source = FileSource { source: Some(source.to_string()), path: "test.huff".to_string(), access: None, dependencies: vec![] };

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new(&evm_version, Arc::new(vec!["test.huff".to_string()]), None, None, None, None, None, false, false);

    let arc_source = Arc::new(full_source);
    match compiler.gen_artifact(Arc::clone(&arc_source)) {
        Ok(artifact) => {
            assert!(artifact.runtime_map.is_some());
            let runtime_map = artifact.runtime_map.unwrap();

            // Check exact positions and content
            // "0x05" from MAIN
            assert_eq!(runtime_map[0].source_start, 208);
            assert_eq!(&source[208..212], "0x05");

            // "0x03" from MAIN
            assert_eq!(runtime_map[1].source_start, 217);
            assert_eq!(&source[217..221], "0x03");

            // "<const>" placeholder from ADD_CONST
            assert_eq!(runtime_map[2].source_start, 60);
            assert_eq!(&source[60..67], "<const>");

            // "add" from ADD_CONST definition
            assert_eq!(runtime_map[3].source_start, 72);
            assert_eq!(&source[72..75], "add");

            // "<const>" placeholder from MUL_CONST
            assert_eq!(runtime_map[4].source_start, 139);
            assert_eq!(&source[139..146], "<const>");

            // "mul" from MUL_CONST definition
            assert_eq!(runtime_map[5].source_start, 151);
            assert_eq!(&source[151..154], "mul");

            // "0x00" from MAIN
            assert_eq!(runtime_map[6].source_start, 266);
            assert_eq!(&source[266..270], "0x00");

            // "mstore" from MAIN
            assert_eq!(runtime_map[7].source_start, 275);
            assert_eq!(&source[275..281], "mstore");

            // Verify that PC values are sequential based on instruction sizes
            // We can't verify exact sizes without knowing bytecode lengths,
            // but we can verify they're monotonically increasing
            let mut last_pc = 0;
            for (i, entry) in runtime_map.iter().enumerate() {
                if i > 0 {
                    assert!(entry.pc > last_pc, "PC should increase: {} <= {}", entry.pc, last_pc);
                }
                last_pc = entry.pc;
            }
        }
        Err(e) => panic!("Failed to generate artifact: {}", e),
    }
}

#[test]
fn test_nested_imports_source_mapping() {
    use std::collections::HashMap;

    // Create an in-memory file system with nested imports
    let mut files = HashMap::new();

    // Inner file (deepest dependency)
    files.insert(
        "inner.huff".to_string(),
        r#"#define macro INNER() = takes(1) returns(1) {
    0x01
    add
}"#
        .to_string(),
    );

    // Middle file that imports inner
    files.insert(
        "middle.huff".to_string(),
        r#"#include "inner.huff"

#define macro MIDDLE() = takes(1) returns(1) {
    INNER()
    0x02
    mul
}"#
        .to_string(),
    );

    // Main file that imports middle (nested import: main -> middle -> inner)
    let main_source = r#"#include "middle.huff"

#define macro MAIN() = takes(0) returns(0) {
    0x42
    MIDDLE()
    0x00
    sstore
}"#;
    files.insert("main.huff".to_string(), main_source.to_string());

    let evm_version = EVMVersion::default();
    let compiler = Compiler::new_in_memory(&evm_version, Arc::new(vec!["main.huff".to_string()]), files, None, None, None, None, false);

    // Fetch and compile the main file with dependencies
    use huff_neo_utils::file::remapper::Remapper;
    use std::path::PathBuf;

    let files_result = Compiler::fetch_sources(vec![PathBuf::from("main.huff")], compiler.file_provider.clone());
    let file = files_result.into_iter().next().unwrap().unwrap();

    // Resolve dependencies
    let remapper = Remapper::new("./");
    let file_with_deps = Compiler::recurse_deps(file, &remapper, compiler.file_provider.clone(), std::collections::HashSet::new()).unwrap();

    let artifact = compiler.gen_artifact(file_with_deps).unwrap();

    assert!(artifact.runtime_map.is_some());
    let runtime_map = artifact.runtime_map.unwrap();

    // 0x42, 0x01, add, 0x02, mul, 0x00, sstore
    assert_eq!(runtime_map.len(), 7);

    // 0x42
    assert_eq!(runtime_map[0].ic, 0);
    assert_eq!(runtime_map[0].pc, 0);
    assert_eq!(runtime_map[0].bytecode_length, 2); // PUSH1 0x42
    assert_eq!(runtime_map[0].source_length, 4);

    // 0x01
    assert_eq!(runtime_map[1].ic, 1);
    assert_eq!(runtime_map[1].pc, 2);
    assert_eq!(runtime_map[1].bytecode_length, 2); // PUSH1 0x01
    assert_eq!(runtime_map[1].source_length, 4);

    // add
    assert_eq!(runtime_map[2].ic, 2);
    assert_eq!(runtime_map[2].pc, 4);
    assert_eq!(runtime_map[2].bytecode_length, 1); // ADD
    assert_eq!(runtime_map[2].source_length, 3);

    // 0x02
    assert_eq!(runtime_map[3].ic, 3);
    assert_eq!(runtime_map[3].pc, 5);
    assert_eq!(runtime_map[3].bytecode_length, 2); // PUSH1 0x02
    assert_eq!(runtime_map[3].source_length, 4);

    // mul
    assert_eq!(runtime_map[4].ic, 4);
    assert_eq!(runtime_map[4].pc, 7);
    assert_eq!(runtime_map[4].bytecode_length, 1); // MUL
    assert_eq!(runtime_map[4].source_length, 3);

    // 0x00
    assert_eq!(runtime_map[5].ic, 5);
    assert_eq!(runtime_map[5].pc, 8);
    assert_eq!(runtime_map[5].bytecode_length, 1); // PUSH0
    assert_eq!(runtime_map[5].source_length, 4);

    // sstore
    assert_eq!(runtime_map[6].ic, 6);
    assert_eq!(runtime_map[6].pc, 9);
    assert_eq!(runtime_map[6].bytecode_length, 1); // SSTORE
    assert_eq!(runtime_map[6].source_length, 6);

    // File IDs (inner=0, middle=1, main=2)
    assert_eq!(runtime_map[0].file_id, 2); // 0x42 from main
    assert_eq!(runtime_map[1].file_id, 0); // 0x01 from inner
    assert_eq!(runtime_map[2].file_id, 0); // add from inner
    assert_eq!(runtime_map[3].file_id, 1); // 0x02 from middle
    assert_eq!(runtime_map[4].file_id, 1); // mul from middle
    assert_eq!(runtime_map[5].file_id, 2); // 0x00 from main
    assert_eq!(runtime_map[6].file_id, 2); // sstore from main

    // Source positions
    assert_eq!(runtime_map[0].source_start, 73); // 0x42
    assert_eq!(runtime_map[1].source_start, 50); // 0x01
    assert_eq!(runtime_map[2].source_start, 59); // add
    assert_eq!(runtime_map[3].source_start, 86); // 0x02
    assert_eq!(runtime_map[4].source_start, 95); // mul
    assert_eq!(runtime_map[5].source_start, 95); // 0x00
    assert_eq!(runtime_map[6].source_start, 104); // sstore
}
