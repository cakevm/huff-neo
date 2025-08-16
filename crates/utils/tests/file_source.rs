use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::prelude::Span;
use huff_neo_utils::time::Time;
use std::sync::Arc;

#[test]
fn test_fully_flatten() {
    let file_d = Arc::new(FileSource {
        path: "d.txt".to_string(),
        source: Some("content of d ".to_string()),
        access: Some(Time::now()),
        dependencies: vec![],
    });

    let file_b = Arc::new(FileSource {
        path: "b.txt".to_string(),
        source: Some("content of b ".to_string()),
        access: Some(Time::now()),
        dependencies: vec![file_d.clone()],
    });

    let file_c = Arc::new(FileSource {
        path: "c.txt".to_string(),
        source: Some("content of c ".to_string()),
        access: Some(Time::now()),
        dependencies: vec![],
    });

    let file_a_with_deps = Arc::new(FileSource {
        path: "a.txt".to_string(),
        source: Some("content of a ".to_string()),
        access: Some(Time::now()),
        dependencies: vec![file_b.clone(), file_c.clone(), file_d.clone()],
    });

    let (flattened_source, relative_positions) = FileSource::fully_flatten(file_a_with_deps);

    assert_eq!(flattened_source, "content of d \ncontent of b \ncontent of c \ncontent of a ");
    assert_eq!(relative_positions.len(), 4);

    assert_eq!(relative_positions.first().unwrap().1.start, 0);
    assert_eq!(relative_positions.first().unwrap().1.end, 12);

    assert_eq!(relative_positions.get(1).unwrap().1.start, 14);
    assert_eq!(relative_positions.get(1).unwrap().1.end, 26);

    assert_eq!(relative_positions.get(2).unwrap().1.start, 28);
    assert_eq!(relative_positions.get(2).unwrap().1.end, 40);

    assert_eq!(relative_positions.get(3).unwrap().1.start, 42);
    assert_eq!(relative_positions.get(3).unwrap().1.end, 54);
}

#[test]
fn test_source_seg() {
    let span = Span {
        start: 59,
        end: 67,
        file: Some(Arc::new(
            FileSource {
                path: "./huff-examples/errors/error.huff".to_string(),
                source: Some("#include \"./import.huff\"\n\n#define function addressGetter() internal returns (address)".to_string()),
                access: None,
                dependencies: vec![
                    Arc::new(FileSource {
                        path: "./huff-examples/errors/import.huff".to_string(),
                        source: Some("#define macro SOME_RANDOM_MACRO() = takes(2) returns (1) {\n    // Store the keys in memory\n    dup1 0x00 mstore\n    swap1 dup1 0x00 mstore\n\n    // Hash the data, generating a key.\n    0x40 sha3\n}\n".to_string()),
                        access: None,
                        dependencies: vec![]
                    })
                ]
            }
        ))
    };

    let source_seg = span.source_seg();
    assert_eq!(
        source_seg,
        format!("\n     {}|\n  > {} | {}\n     {}|", " ", 3, "#define function addressGetter() internal returns (address)", " ",)
    );
}

#[test]
fn test_derive_dir() {
    let localized = FileSource::derive_dir("./examples/ERC20.huff").unwrap();
    assert_eq!(localized, "./examples");
    let localized = FileSource::derive_dir("./ERC20.huff").unwrap();
    assert_eq!(localized, ".");
    let localized = FileSource::derive_dir("ERC20.huff").unwrap();
    assert_eq!(localized, "");
}

#[test]
fn test_localize_file() {
    let localized = FileSource::localize_file("./examples/ERC20.huff", "./utilities/Address.huff").unwrap();
    assert_eq!(localized, "./examples/utilities/Address.huff");
    let localized = FileSource::localize_file("./ERC20.huff", "./utilities/Address.huff").unwrap();
    assert_eq!(localized, "./utilities/Address.huff");
    let localized = FileSource::localize_file("ERC20.huff", "./utilities/Address.huff").unwrap();
    assert_eq!(localized, "./utilities/Address.huff");
    let localized = FileSource::localize_file("ERC20.huff", "./Address.huff").unwrap();
    assert_eq!(localized, "./Address.huff");
    let localized = FileSource::localize_file("ERC20.huff", "Address.huff").unwrap();
    assert_eq!(localized, "./Address.huff");
    let localized = FileSource::localize_file("./ERC20.huff", "Address.huff").unwrap();
    assert_eq!(localized, "./Address.huff");
    let localized = FileSource::localize_file("./examples/ERC20.huff", "Address.huff").unwrap();
    assert_eq!(localized, "./examples/Address.huff");
    let localized = FileSource::localize_file("./examples/ERC20.huff", "../Address.huff").unwrap();
    assert_eq!(localized, "./Address.huff");
    let localized = FileSource::localize_file("./examples/ERC20.huff", "../../Address.huff").unwrap();
    assert_eq!(localized, "../Address.huff");
    let localized = FileSource::localize_file("./examples/ERC20.huff", "../../../Address.huff").unwrap();
    assert_eq!(localized, "../../Address.huff");
    let localized = FileSource::localize_file("../examples/ERC20.huff", "../../../Address.huff").unwrap();
    assert_eq!(localized, "../../../Address.huff");
    let localized = FileSource::localize_file("../examples/ERC20.huff", "./Address.huff").unwrap();
    assert_eq!(localized, "../examples/Address.huff");
    let localized = FileSource::localize_file("../examples/ERC20.huff", "Address.huff").unwrap();
    assert_eq!(localized, "../examples/Address.huff");
    let localized = FileSource::localize_file("../../examples/ERC20.huff", "Address.huff").unwrap();
    assert_eq!(localized, "../../examples/Address.huff");
    let localized = FileSource::localize_file("../../examples/ERC20.huff", "../Address.huff").unwrap();
    assert_eq!(localized, "../../Address.huff");
    let localized = FileSource::localize_file("../../examples/ERC20.huff", "../../../Address.huff").unwrap();
    assert_eq!(localized, "../../../../Address.huff");
    let localized = FileSource::localize_file("examples/ERC20.huff", "../random_dir/Address.huff").unwrap();
    assert_eq!(localized, "./random_dir/Address.huff");
}
