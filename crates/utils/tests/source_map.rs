use huff_neo_utils::artifact::SourceMapEntry;

#[test]
fn test_solidity_source_map_format() {
    // Test with simple source map entries
    let entries = vec![
        SourceMapEntry { ic: 0, pc: 0, bytecode_length: 2, source_start: 100, source_length: 10, file_id: 0 },
        SourceMapEntry { ic: 1, pc: 2, bytecode_length: 4, source_start: 110, source_length: 10, file_id: 0 },
        SourceMapEntry { ic: 2, pc: 6, bytecode_length: 1, source_start: 120, source_length: 5, file_id: 0 },
    ];

    let solidity_format = SourceMapEntry::generate_solidity_source_map(&entries);

    // Expected format: "100:10:0:-;110:::-;120:5::-"
    // First entry: 100 (start), 10 (length), 0 (file), - (jump)
    // Second entry: 110 (start changed), same length (omitted), same file (omitted), - (jump)
    // Third entry: 120 (start changed), 5 (length changed), same file (omitted), - (jump)
    assert_eq!(solidity_format, "100:10:0:-;110:::-;120:5::-");
}

#[test]
fn test_solidity_source_map_format_empty() {
    let entries = vec![];
    let solidity_format = SourceMapEntry::generate_solidity_source_map(&entries);
    assert_eq!(solidity_format, "");
}

#[test]
fn test_solidity_source_map_format_single() {
    let entries = vec![SourceMapEntry { ic: 0, pc: 0, bytecode_length: 2, source_start: 50, source_length: 25, file_id: 0 }];

    let solidity_format = SourceMapEntry::generate_solidity_source_map(&entries);
    assert_eq!(solidity_format, "50:25:0:-");
}

#[test]
fn test_solidity_source_map_format_same_values() {
    // Test when consecutive entries have same values (should be omitted)
    let entries = vec![
        SourceMapEntry { ic: 0, pc: 0, bytecode_length: 2, source_start: 100, source_length: 10, file_id: 0 },
        SourceMapEntry {
            ic: 1,
            pc: 2,
            bytecode_length: 2,
            source_start: 100, // Same start
            source_length: 10, // Same length
            file_id: 0,
        },
        SourceMapEntry {
            ic: 2,
            pc: 4,
            bytecode_length: 2,
            source_start: 100, // Still same
            source_length: 10, // Still same
            file_id: 0,
        },
    ];

    let solidity_format = SourceMapEntry::generate_solidity_source_map(&entries);
    // All values same as first, so omitted except jump type
    assert_eq!(solidity_format, "100:10:0:-;:::-;:::-");
}
