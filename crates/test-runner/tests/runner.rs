use alloy_primitives::U256;
use foundry_evm::backend::DatabaseError;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_test_runner::prelude::{RunnerError, TestResult, TestRunner, TestStatus};
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::{Contract, Token};
use revm::database::{CacheDB, EmptyDBTyped};
use revm::primitives::hardfork::SpecId;

#[test]
fn test_runner_return() {
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    let mut runner = TestRunner::default();
    let code = "602060005260206000F3";
    let deployed_addr = runner.deploy_code(&mut db, code.to_string()).unwrap();
    let result = runner.call(String::from("RETURN"), &mut db, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(result.name, "RETURN");
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.gas, 18);
    assert_eq!(result.return_data, Some("0000000000000000000000000000000000000000000000000000000000000020".to_string()));
}

#[test]
fn test_runner_stop() {
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    let mut runner = TestRunner::default();
    let code = "00";
    let deployed_addr = runner.deploy_code(&mut db, code.to_string()).unwrap();
    let result = runner.call(String::from("STOP"), &mut db, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(result.name, "STOP");
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.gas, 0);
    assert_eq!(result.return_data, None);
}

#[test]
fn test_runner_revert() {
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    let mut runner = TestRunner::default();
    let code = "60006000FD";
    let deployed_addr = runner.deploy_code(&mut db, code.to_string()).unwrap();
    let result = runner.call(String::from("REVERT"), &mut db, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(result.name, "REVERT");
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Revert));
    assert_eq!(result.gas, 6);
    assert_eq!(result.return_data, None);
}

/// Test runner configured for the given hardfork, including its gas schedule.
fn runner_with_spec(spec: SpecId) -> TestRunner {
    let mut runner = TestRunner::default();
    runner.env.evm_env.cfg_env.set_spec_and_mainnet_gas_params(spec);
    runner
}

/// PUSH1 18 .. PUSH1 1, DUPN 17 (immediate 0x80), then return the top stack item
fn dupn_code() -> String {
    let pushes: String = (1..=18u8).rev().map(|i| format!("60{i:02x}")).collect();
    format!("{pushes}e68060005260206000f3")
}

#[test]
fn test_runner_amsterdam_dupn() {
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    let mut runner = runner_with_spec(SpecId::AMSTERDAM);
    let deployed_addr = runner.deploy_code(&mut db, dupn_code()).unwrap();
    let result = runner.call(String::from("DUPN"), &mut db, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.return_data, Some(format!("{:064x}", 17)));
}

#[test]
fn test_runner_pre_amsterdam_dupn_halts() {
    // Before Amsterdam 0xe6 is not a valid opcode, so the same code must halt
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    let mut runner = runner_with_spec(SpecId::OSAKA);
    let deployed_addr = runner.deploy_code(&mut db, dupn_code()).unwrap();
    let result = runner.call(String::from("DUPN"), &mut db, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Revert));
    assert_eq!(result.return_data, None);
    assert!(result.revert_reason.is_some(), "expected a halt reason");
}

#[test]
fn test_runner_gas_excludes_intrinsic_cost() {
    // The intrinsic transaction cost is repriced from Amsterdam (EIP-2780), including an extra
    // charge for value transfers; reported gas must only cover execution on either side of the fork
    for spec in [SpecId::OSAKA, SpecId::AMSTERDAM] {
        for value in [U256::ZERO, U256::from(1)] {
            let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
            let mut runner = runner_with_spec(spec);
            let deployed_addr = runner.deploy_code(&mut db, "00".to_string()).unwrap();
            let result = runner.call(String::from("STOP"), &mut db, deployed_addr, value, String::default()).unwrap();
            assert_eq!(result.gas, 0, "STOP on {spec:?} with value {value}");

            let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
            let mut runner = runner_with_spec(spec);
            let deployed_addr = runner.deploy_code(&mut db, "602060005260206000F3".to_string()).unwrap();
            let result = runner.call(String::from("RETURN"), &mut db, deployed_addr, value, String::default()).unwrap();
            assert_eq!(result.gas, 18, "RETURN on {spec:?} with value {value}");
        }
    }
}

/// Parses a source file into a contract.
fn parse_contract(source: &str) -> Contract {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let tokens = Lexer::new(flattened_source).map(|x| x.unwrap()).collect::<Vec<Token>>();
    Parser::new(tokens, None).parse().unwrap()
}

/// Runs the named test macro of the source.
fn run_source_test(source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    let contract = parse_contract(source);
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    TestRunner::default().run_test(&mut db, contract.macros.get(name).unwrap(), &contract).map_err(Box::new)
}

/// Return data of a test that returns its callvalue and first calldata word
fn value_and_calldata_word(value: u64, calldata: &str) -> String {
    format!("{value:064x}{calldata:0<64}")
}

#[test]
fn test_runner_applies_calldata_and_value_decorators() {
    let source = r#"
    #[calldata("0x0102"), value(0x05)]
    #define test T() = {
        callvalue 0x00 mstore
        0x00 calldataload 0x20 mstore
        0x40 0x00 return
    }
    "#;

    let result = run_source_test(source, "T").unwrap();
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.return_data, Some(value_and_calldata_word(5, "0102")));
}

#[test]
fn test_runner_applies_stacked_decorator_blocks() {
    let source = r#"
    #[calldata("0x0102")]
    #[value(0x05),]
    #define test T() = {
        callvalue 0x00 mstore
        0x00 calldataload 0x20 mstore
        0x40 0x00 return
    }
    "#;

    let result = run_source_test(source, "T").unwrap();
    assert_eq!(result.return_data, Some(value_and_calldata_word(5, "0102")));
}

#[test]
fn test_runner_rejects_unknown_decorator_flag() {
    let source = r#"
    #[expect_nothing(0x01)]
    #define test T() = { stop }
    "#;

    let err = run_source_test(source, "T").unwrap_err();
    assert!(
        matches!(err.as_ref(), RunnerError::InvalidDecorator(msg) if msg.contains("Unknown decorator flag: expect_nothing")),
        "{err:?}"
    );
}

#[test]
fn test_runner_rejects_wrong_decorator_arguments() {
    for flag in ["calldata(0x01)", "calldata(\"0x01\", \"0x02\")", "value(\"0x01\")", "value(v = 0x01)", "value"] {
        let source = format!("#[{flag}]\n#define test T() = {{ stop }}");
        let err = run_source_test(&source, "T").unwrap_err();
        assert!(matches!(*err, RunnerError::InvalidDecorator(_)), "{flag}: {err:?}");
    }
}
