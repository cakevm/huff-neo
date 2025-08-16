use huff_neo_core::Compiler;
use huff_neo_utils::file::file_provider::InMemoryFileProvider;
use huff_neo_utils::file::file_source::FileSource;
use std::collections::HashMap;
use std::sync::Arc;

#[test]
fn test_flattened_source_removes_includes() {
    // Create a simple contract with includes
    let mut file_sources = HashMap::new();

    // Main file with includes
    file_sources.insert(
        "./main.huff".to_string(),
        r#"#include "./constants.huff"
#include "./macros.huff"

#define macro MAIN() = takes(0) returns(0) {
    0x00 0x00 revert
}"#
        .to_string(),
    );

    // Constants file
    file_sources
        .insert("./constants.huff".to_string(), r#"#define constant OWNER = 0x1234567890123456789012345678901234567890"#.to_string());

    // Macros file
    file_sources.insert(
        "./macros.huff".to_string(),
        r#"#define macro ADD() = takes(2) returns(1) {
    add
}"#
        .to_string(),
    );

    let file_provider = Arc::new(InMemoryFileProvider::new(file_sources));

    // Create a FileSource for the main file
    let main_file = Arc::new(FileSource {
        path: "./main.huff".to_string(),
        source: Some(
            r#"#include "./constants.huff"
#include "./macros.huff"

#define macro MAIN() = takes(0) returns(0) {
    0x00 0x00 revert
}"#
            .to_string(),
        ),
        access: None,
        dependencies: vec![],
    });

    // Get flattened source
    let result = Compiler::get_flattened_source(main_file, file_provider);

    assert!(result.is_ok());
    let flattened = result.unwrap();

    // Check that #include statements are commented out
    assert!(flattened.contains("// #include"));
    assert!(!flattened.contains("\n#include")); // No uncommented includes

    // Check that the actual content from included files is present
    assert!(flattened.contains("#define constant OWNER"));
    assert!(flattened.contains("#define macro ADD()"));
    assert!(flattened.contains("#define macro MAIN()"));
}

#[test]
fn test_flattened_source_handles_inline_includes() {
    // Test case where #include appears on same line as other content
    let mut file_sources = HashMap::new();

    file_sources.insert(
        "./main.huff".to_string(),
        r#"#define macro FOO() = takes(0) returns(0) {
    0x00
}#include "./other.huff"
#define macro BAR() = takes(0) returns(0) {
    0x01
}"#
        .to_string(),
    );

    file_sources.insert("./other.huff".to_string(), r#"#define constant VALUE = 0x42"#.to_string());

    let file_provider = Arc::new(InMemoryFileProvider::new(file_sources));

    let main_file = Arc::new(FileSource {
        path: "./main.huff".to_string(),
        source: Some(
            r#"#define macro FOO() = takes(0) returns(0) {
    0x00
}#include "./other.huff"
#define macro BAR() = takes(0) returns(0) {
    0x01
}"#
            .to_string(),
        ),
        access: None,
        dependencies: vec![],
    });

    let result = Compiler::get_flattened_source(main_file, file_provider);

    assert!(result.is_ok());
    let flattened = result.unwrap();

    // Check that #include is commented out even when it's on the same line as other content
    assert!(flattened.contains("// #include"));
    assert!(!flattened.contains("}#include")); // The problematic case should be fixed

    // The closing brace should still be there
    assert!(flattened.contains("}"));
    assert!(flattened.contains("#define constant VALUE"));
}

#[test]
fn test_flattened_source_preserves_non_include_directives() {
    // Test that other preprocessor directives are preserved
    let mut file_sources = HashMap::new();

    file_sources.insert(
        "./main.huff".to_string(),
        r#"#include "./helper.huff"
#define macro MAIN() = takes(0) returns(0) {
    #define constant LOCAL = 0x01
    0x00 0x00 revert
}"#
        .to_string(),
    );

    file_sources.insert("./helper.huff".to_string(), r#"#define function transfer(address,uint256) nonpayable returns()"#.to_string());

    let file_provider = Arc::new(InMemoryFileProvider::new(file_sources));

    let main_file = Arc::new(FileSource {
        path: "./main.huff".to_string(),
        source: Some(
            r#"#include "./helper.huff"
#define macro MAIN() = takes(0) returns(0) {
    #define constant LOCAL = 0x01
    0x00 0x00 revert
}"#
            .to_string(),
        ),
        access: None,
        dependencies: vec![],
    });

    let result = Compiler::get_flattened_source(main_file, file_provider);

    assert!(result.is_ok());
    let flattened = result.unwrap();

    // #include should be commented out
    assert!(flattened.contains("// #include"));
    assert!(!flattened.contains("\n#include")); // No uncommented includes on new lines

    // But other #define directives should be preserved
    assert!(flattened.contains("#define macro MAIN()"));
    assert!(flattened.contains("#define constant LOCAL"));
    assert!(flattened.contains("#define function transfer"));
}
