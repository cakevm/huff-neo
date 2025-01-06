use alloy_dyn_abi::{DynSolValue, Word};
use alloy_primitives::{Address, I256, U256};
use huff_neo_codegen::Codegen;
use huff_neo_utils::bytes_util::*;
use std::str::FromStr;

#[test]
fn encode_simple_constructor_args() {
    let expected_address: [u8; 20] = [100, 109, 184, 255, 194, 30, 125, 220, 43, 99, 39, 68, 141, 217, 250, 86, 13, 244, 16, 135];
    let expected_bytes32: Vec<u8> = str_to_vec("87674fa174add091f082eab424cc60625118fa4c553592a4e54a76fb9e8512f6").unwrap();
    // Bogus constructors args
    let args: Vec<String> = [
        "Hello",
        "10000",
        "false",
        "0x646dB8ffC21e7ddc2B6327448dd9Fa560Df41087",
        "0x87674fa174add091f082eab424cc60625118fa4c553592a4e54a76fb9e8512f6",
        "-10",
        "+55",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let results = Codegen::encode_constructor_args(args);
    assert_eq!(results[0], DynSolValue::String("Hello".to_string()));
    assert_eq!(results[1], DynSolValue::Uint(U256::from_str("10000").unwrap(), 256));
    assert_eq!(results[2], DynSolValue::Bool(false));
    assert_eq!(results[3], DynSolValue::Address(Address::from(expected_address)));
    assert_eq!(results[4], DynSolValue::FixedBytes(Word::from_slice(&expected_bytes32), 32));
    assert_eq!(results[5], DynSolValue::Int(I256::from_str("-10").unwrap(), 256));
    assert_eq!(results[6], DynSolValue::Int(I256::from_str("+55").unwrap(), 256));
}

#[test]
fn encode_array_constructor_args() {
    let expected_address: [u8; 20] = [100, 109, 184, 255, 194, 30, 125, 220, 43, 99, 39, 68, 141, 217, 250, 86, 13, 244, 16, 135];
    let _expected_bytes32: Vec<u8> = str_to_vec("87674fa174add091f082eab424cc60625118fa4c553592a4e54a76fb9e8512f6").unwrap();
    // Bogus constructors args
    let args: Vec<String> = [
        "[100, 200, 300]",
        "[0x646dB8ffC21e7ddc2B6327448dd9Fa560Df41087, 0x646dB8ffC21e7ddc2B6327448dd9Fa560Df41087]",
        "[true, false, false]",
        "[Hello, World, Yes]",
        "[\"Hello\", \"World\", \"Yes\"]",
        "['Hello', 'World', 'Yes']",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let results = Codegen::encode_constructor_args(args);

    assert_eq!(
        results[0],
        DynSolValue::Array(vec![
            DynSolValue::Uint(U256::from_str("100").unwrap(), 256),
            DynSolValue::Uint(U256::from_str("200").unwrap(), 256),
            DynSolValue::Uint(U256::from_str("300").unwrap(), 256),
        ])
    );
    assert_eq!(
        results[1],
        DynSolValue::Array(vec![
            DynSolValue::Address(Address::from(expected_address)),
            DynSolValue::Address(Address::from(expected_address)),
        ])
    );
    assert_eq!(results[2], DynSolValue::Array(vec![DynSolValue::Bool(true), DynSolValue::Bool(false), DynSolValue::Bool(false),]));
    let expected_array = DynSolValue::Array(vec![
        DynSolValue::String("Hello".to_string()),
        DynSolValue::String("World".to_string()),
        DynSolValue::String("Yes".to_string()),
    ]);
    assert_eq!(results[3], expected_array);
    assert_eq!(results[4], expected_array);
    assert_eq!(results[5], expected_array);
}

#[test]
fn encode_missing_brackets_array_constructor_args() {
    let expected_address: [u8; 20] = [100, 109, 184, 255, 194, 30, 125, 220, 43, 99, 39, 68, 141, 217, 250, 86, 13, 244, 16, 135];
    let _expected_bytes32: Vec<u8> = str_to_vec("87674fa174add091f082eab424cc60625118fa4c553592a4e54a76fb9e8512f6").unwrap();
    // Bogus constructors args
    let args: Vec<String> = [
        "  100,  200,  300   ",
        " 0x646dB8ffC21e7ddc2B6327448dd9Fa560Df41087,    0x646dB8ffC21e7ddc2B6327448dd9Fa560Df41087",
        "true,  false,   false",
        "Hello, World, Yes",
        "   \"Hello\", \"World\", \"Yes\"",
        "'Hello',  'World', 'Yes'   ",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let results = Codegen::encode_constructor_args(args);

    assert_eq!(
        results[0],
        DynSolValue::Array(vec![
            DynSolValue::Uint(U256::from_str("100").unwrap(), 256),
            DynSolValue::Uint(U256::from_str("200").unwrap(), 256),
            DynSolValue::Uint(U256::from_str("300").unwrap(), 256),
        ])
    );
    assert_eq!(
        results[1],
        DynSolValue::Array(vec![
            DynSolValue::Address(Address::from(expected_address)),
            DynSolValue::Address(Address::from(expected_address)),
        ])
    );
    assert_eq!(results[2], DynSolValue::Array(vec![DynSolValue::Bool(true), DynSolValue::Bool(false), DynSolValue::Bool(false),]));
    let expected_array = DynSolValue::Array(vec![
        DynSolValue::String("Hello".to_string()),
        DynSolValue::String("World".to_string()),
        DynSolValue::String("Yes".to_string()),
    ]);
    assert_eq!(results[3], expected_array);
    assert_eq!(results[4], expected_array);
    assert_eq!(results[5], expected_array);
}
