use alloy_primitives::{U256, address, hex};
use foundry_evm::backend::DatabaseError;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_test_runner::prelude::{DecoratorError, DecoratorErrorKind, RunnerError, TestResult, TestRunner, TestStatus};
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::{Contract, EVMVersion, SupportedEVMVersions, Token};
use revm::Database;
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

/// In-memory database the source tests run against
type TestDb = CacheDB<EmptyDBTyped<DatabaseError>>;

/// Parses a source file into a contract.
fn parse_contract(source: &str) -> Contract {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let tokens = Lexer::new(flattened_source).map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut contract = Parser::new(tokens, None).parse().unwrap();
    contract.derive_storage_pointers();
    contract
}

/// Runs the named test macro of the source with the given runner and database.
fn run_source_test_with(runner: &mut TestRunner, db: &mut TestDb, source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    let contract = parse_contract(source);
    runner.run_test(db, contract.macros.get(name).unwrap(), &contract).map_err(Box::new)
}

/// Runs the named test macro of the source against the given database.
fn run_source_test_in(db: &mut TestDb, source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    run_source_test_with(&mut TestRunner::default(), db, source, name)
}

/// Runs the named test macro of the source.
fn run_source_test(source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    run_source_test_in(&mut TestDb::default(), source, name)
}

/// The decorator error of test `T` in a source whose decorators cannot be applied.
fn decorator_error(source: &str) -> DecoratorError {
    match *run_source_test(source, "T").unwrap_err() {
        RunnerError::InvalidDecorator(err) => err,
        err => panic!("expected a decorator error, got {err:?}"),
    }
}

/// Return data of a test that returns its callvalue and first calldata word
fn value_and_calldata_word(value: u64, calldata: &str) -> String {
    format!("{value:064x}{calldata:0<64}")
}

/// A test macro that returns its whole calldata
const RETURN_CALLDATA: &str = "#define test T() = { calldatasize 0x00 0x00 calldatacopy calldatasize 0x00 return }";

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
fn test_runner_builds_calldata_from_byte_sequence() {
    // Selectors add 4 bytes, hex literals and strings keep their width, everything else is a word
    let source = format!(
        r#"
        #define error Unauthorized()
        #[calldata(__FUNC_SIG("transfer(address,uint256)"), 0x0001, 42, __ERROR(Unauthorized), "0xff")]
        {RETURN_CALLDATA}
        "#
    );

    let result = run_source_test(&source, "T").unwrap();
    assert_eq!(result.return_data, Some(format!("a9059cbb0001{:064x}82b42900ff", 42)));
}

#[test]
fn test_runner_value_accepts_any_word() {
    let source = r#"
    #define constant AMOUNT = 0x03e8
    #[value([AMOUNT])]
    #define test T() = { callvalue 0x00 mstore 0x20 0x00 return }
    "#;

    let result = run_source_test(source, "T").unwrap();
    assert_eq!(result.return_data, Some(format!("{:064x}", 1000)));
}

#[test]
fn test_runner_stores_slots_of_the_test() {
    let source = r#"
    #define function transfer(address,uint256) nonpayable returns ()
    #define constant OWNER_SLOT = FREE_STORAGE_POINTER()
    #define constant COUNT = 0x07
    #define macro MAIN() = { [OWNER_SLOT] sload }
    #[store([OWNER_SLOT], this), store(0x01, [COUNT]), store(2, __FUNC_SIG(transfer))]
    #[store(0x03, true, at = this)]
    #define test T() = {
        [OWNER_SLOT] sload 0x00 mstore
        0x01 sload 0x20 mstore
        0x02 sload 0x40 mstore
        0x03 sload 0x60 mstore
        0x80 0x00 return
    }
    "#;

    let result = run_source_test(source, "T").unwrap();
    let this = result.address.unwrap();
    assert_eq!(result.return_data, Some(format!("{:0>64}{:064x}{:064x}{:064x}", hex::encode(this), 7, 0xa9059cbbu32, 1)));
}

#[test]
fn test_runner_stores_slots_of_other_addresses() {
    // The same slot may be set once per address
    let source = r#"
    #[store(0x01, 0x2a, at = 0x1000), store(0x01, 0x2b)]
    #define test T() = { 0x01 sload 0x00 mstore 0x20 0x00 return }
    "#;

    let mut db = TestDb::default();
    let result = run_source_test_in(&mut db, source, "T").unwrap();
    assert_eq!(result.return_data, Some(format!("{:064x}", 0x2b)));
    assert_eq!(db.storage(address!("0000000000000000000000000000000000001000"), U256::from(1)).unwrap(), U256::from(0x2a));
}

#[test]
fn test_runner_rejects_storing_a_slot_twice() {
    // `this` is the default address, so both flags set slot 1 of the test
    let err = decorator_error("#[store(0x01, 0x01)]\n#[store(1, 0x02, at = this)]\n#define test T() = { stop }");
    assert_eq!(err.flag, "store");
    assert_eq!(err.kind, DecoratorErrorKind::Duplicate);
    assert_eq!(err.span.0.len(), 2, "the error points at both flags");
}

#[test]
fn test_runner_rejects_repeated_single_use_flag() {
    let err = decorator_error("#[value(0x01), value(0x02)]\n#define test T() = { stop }");
    assert_eq!(err.flag, "value");
    assert_eq!(err.kind, DecoratorErrorKind::Duplicate);
}

#[test]
fn test_runner_rejects_unknown_decorator_flag() {
    let err = decorator_error("#[expect_nothing(0x01)]\n#define test T() = { stop }");
    assert_eq!(err.kind, DecoratorErrorKind::UnknownFlag);
    assert!(err.hint.contains("Unknown decorator flag: expect_nothing"));
}

#[test]
fn test_runner_rejects_wrong_decorator_arguments() {
    let flags = [
        "calldata",
        "value",
        "value(v = 0x01)",
        "store(0x01)",
        "store(0x01, 0x02, 0x03)",
        "store(0x01, 0x02, to = 0x03)",
        "store(0x01, 0x02, at = 0x03, at = 0x04)",
    ];
    for flag in flags {
        let err = decorator_error(&format!("#[{flag}]\n#define test T() = {{ stop }}"));
        assert_eq!(err.kind, DecoratorErrorKind::InvalidArguments, "{flag}");
    }
}

#[test]
fn test_runner_rejects_wrong_decorator_values() {
    let flags = [
        "value(\"0x01\")",
        "value(0x01000000000000000000000000000000000000000000000000000000000000000000)",
        "calldata(\"0xzz\")",
        "store(0x01, someone)",
        "store(0x01, 0x02, at = 0x010000000000000000000000000000000000000000)",
    ];
    for flag in flags {
        let err = decorator_error(&format!("#[{flag}]\n#define test T() = {{ stop }}"));
        assert_eq!(err.kind, DecoratorErrorKind::InvalidValue, "{flag}");
    }
}

#[test]
fn test_runner_rejects_undefined_constant_in_decorator() {
    let err = run_source_test("#[store([MISSING], 0x01)]\n#define test T() = { stop }", "T").unwrap_err();
    assert!(matches!(*err, RunnerError::CompilerError(_)), "{err:?}");
}

#[test]
fn test_runner_applies_environment_decorators() {
    let source = r#"
    #[caller(0xbeef), coinbase(0xc0ffee), chain_id(5), gas_price(10)]
    #[block_number(100), timestamp(1700000000), basefee(7), blob_basefee(3)]
    #[prevrandao(0x2a)]
    #define test T() = {
        caller 0x00 mstore
        origin 0x20 mstore
        coinbase 0x40 mstore
        chainid 0x60 mstore
        gasprice 0x80 mstore
        number 0xa0 mstore
        timestamp 0xc0 mstore
        basefee 0xe0 mstore
        blobbasefee 0x100 mstore
        prevrandao 0x120 mstore
        0x140 0x00 return
    }
    "#;

    let result = run_source_test(source, "T").unwrap();
    let words = [0xbeef, 0xbeef, 0xc0ffee, 5, 10, 100, 1700000000, 7, 3, 0x2a];
    assert_eq!(result.return_data, Some(words.iter().map(|w| format!("{w:064x}")).collect::<String>()));
}

#[test]
fn test_runner_applies_slot_number_decorator() {
    let source = r#"
    #[slot_number(12345)]
    #define test T() = { slotnum 0x00 mstore 0x20 0x00 return }
    "#;

    let mut runner = runner_with_spec(SpecId::AMSTERDAM);
    runner.evm_version = EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let result = run_source_test_with(&mut runner, &mut TestDb::default(), source, "T").unwrap();
    assert_eq!(result.return_data, Some(format!("{:064x}", 12345)));
}

#[test]
fn test_runner_applies_gas_limit_decorator() {
    // A cold SSTORE needs more than 20000 gas on top of the 21000 the transaction costs
    let source = "#[gas_limit(30000)]\n#define test T() = { 0x01 0x00 sstore }";

    let result = run_source_test(source, "T").unwrap();
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Revert));
}

#[test]
fn test_runner_resolves_caller_keyword_to_caller_decorator() {
    // `caller` in a value means the test's sender, even when the `caller` flag comes later
    let source = r#"
    #[store(0x01, caller), caller(0xbeef)]
    #define test T() = { 0x01 sload 0x00 mstore 0x20 0x00 return }
    "#;

    let result = run_source_test(source, "T").unwrap();
    assert_eq!(result.return_data, Some(format!("{:064x}", 0xbeef)));
}

#[test]
fn test_runner_rejects_out_of_range_environment_values() {
    for flag in
        ["chain_id(0x010000000000000000)", "basefee(0x010000000000000000)", "coinbase(0x010000000000000000000000000000000000000000)"]
    {
        let err = decorator_error(&format!("#[{flag}]\n#define test T() = {{ stop }}"));
        assert_eq!(err.kind, DecoratorErrorKind::InvalidValue, "{flag}");
    }
}

#[test]
fn test_runner_rejects_repeated_environment_flag() {
    let err = decorator_error("#[timestamp(1)]\n#[timestamp(2)]\n#define test T() = { stop }");
    assert_eq!(err.kind, DecoratorErrorKind::Duplicate);
    assert_eq!(err.span.0.len(), 2);
}
