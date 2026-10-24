use alloy_primitives::{Address, KECCAK256_EMPTY, U256, address, hex, keccak256};
use foundry_evm::core::evm::EthEvmNetwork;
use foundry_evm::executors::Executor;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_test_runner::prelude::{DecoratorError, DecoratorErrorKind, Outcome, RunnerError, TestResult, TestRunner, TestStatus};
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::{Contract, EVMVersion, SupportedEVMVersions, Token};
use revm::DatabaseRef;
use revm::primitives::hardfork::SpecId;

#[test]
fn test_runner_return() {
    let mut runner = TestRunner::default();
    let mut executor = runner.new_executor(None).unwrap();
    let code = "602060005260206000F3";
    let deployed_addr = runner.deploy_code(&mut executor, code.to_string()).unwrap();
    let result = runner.call(String::from("RETURN"), &mut executor, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(result.name, "RETURN");
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.gas, 18);
    assert_eq!(result.return_data, Some("0000000000000000000000000000000000000000000000000000000000000020".to_string()));
}

#[test]
fn test_runner_stop() {
    let mut runner = TestRunner::default();
    let mut executor = runner.new_executor(None).unwrap();
    let code = "00";
    let deployed_addr = runner.deploy_code(&mut executor, code.to_string()).unwrap();
    let result = runner.call(String::from("STOP"), &mut executor, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(result.name, "STOP");
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.gas, 0);
    assert_eq!(result.return_data, None);
}

#[test]
fn test_runner_revert() {
    let mut runner = TestRunner::default();
    let mut executor = runner.new_executor(None).unwrap();
    let code = "60006000FD";
    let deployed_addr = runner.deploy_code(&mut executor, code.to_string()).unwrap();
    let result = runner.call(String::from("REVERT"), &mut executor, deployed_addr, U256::ZERO, String::default()).unwrap();

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
    let mut runner = runner_with_spec(SpecId::AMSTERDAM);
    let mut executor = runner.new_executor(None).unwrap();
    let deployed_addr = runner.deploy_code(&mut executor, dupn_code()).unwrap();
    let result = runner.call(String::from("DUPN"), &mut executor, deployed_addr, U256::ZERO, String::default()).unwrap();

    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.return_data, Some(format!("{:064x}", 17)));
}

#[test]
fn test_runner_pre_amsterdam_dupn_halts() {
    // Before Amsterdam 0xe6 is not a valid opcode, so the same code must halt
    let mut runner = runner_with_spec(SpecId::OSAKA);
    let mut executor = runner.new_executor(None).unwrap();
    let deployed_addr = runner.deploy_code(&mut executor, dupn_code()).unwrap();
    let result = runner.call(String::from("DUPN"), &mut executor, deployed_addr, U256::ZERO, String::default()).unwrap();

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
            let mut runner = runner_with_spec(spec);
            let mut executor = runner.new_executor(None).unwrap();
            let deployed_addr = runner.deploy_code(&mut executor, "00".to_string()).unwrap();
            let result = runner.call(String::from("STOP"), &mut executor, deployed_addr, value, String::default()).unwrap();
            assert_eq!(result.gas, 0, "STOP on {spec:?} with value {value}");

            let mut runner = runner_with_spec(spec);
            let mut executor = runner.new_executor(None).unwrap();
            let deployed_addr = runner.deploy_code(&mut executor, "602060005260206000F3".to_string()).unwrap();
            let result = runner.call(String::from("RETURN"), &mut executor, deployed_addr, value, String::default()).unwrap();
            assert_eq!(result.gas, 18, "RETURN on {spec:?} with value {value}");
        }
    }
}

/// A fresh executor for one test of the default runner
fn new_executor() -> Executor<EthEvmNetwork> {
    TestRunner::default().new_executor(None).unwrap()
}

/// Parses a source file into a contract.
fn parse_contract(source: &str) -> Contract {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let tokens = Lexer::new(flattened_source).map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut contract = Parser::new(tokens, None).parse().unwrap();
    contract.derive_storage_pointers();
    contract
}

/// Runs the named test macro of the source with the given runner, on a fresh executor of that runner.
fn run_source_test_with(runner: &mut TestRunner, source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    let mut executor = runner.new_executor(None).unwrap();
    let contract = parse_contract(source);
    runner.run_test(&mut executor, contract.macros.get(name).unwrap(), &contract).map_err(Box::new)
}

/// Runs the named test macro of the source on the given executor, which keeps the state afterwards.
fn run_source_test_in(executor: &mut Executor<EthEvmNetwork>, source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    let contract = parse_contract(source);
    TestRunner::default().run_test(executor, contract.macros.get(name).unwrap(), &contract).map_err(Box::new)
}

/// Runs the named test macro of the source.
fn run_source_test(source: &str, name: &str) -> Result<TestResult, Box<RunnerError>> {
    run_source_test_in(&mut new_executor(), source, name)
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

    let mut executor = new_executor();
    let result = run_source_test_in(&mut executor, source, "T").unwrap();
    assert_eq!(result.return_data, Some(format!("{:064x}", 0x2b)));
    assert_eq!(
        executor.backend().storage_ref(address!("0000000000000000000000000000000000001000"), U256::from(1)).unwrap(),
        U256::from(0x2a)
    );
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
    let result = run_source_test_with(&mut runner, source, "T").unwrap();
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

/// The result of test `T` in a source whose decorators can be applied.
fn run_test_t(source: &str) -> TestResult {
    run_source_test(source, "T").unwrap()
}

/// The expectation names and messages of the failures of test `T`
fn failures(source: &str) -> Vec<(String, String)> {
    run_test_t(source).failures.into_iter().map(|f| (f.expectation, f.message)).collect()
}

/// A contract with one custom error, followed by the given test.
fn with_error(test: &str) -> String {
    format!("#define error Unauthorized()\n{test}")
}

/// The address `0x1000`, which the account tests use as a second account
const OTHER: Address = address!("0000000000000000000000000000000000001000");

/// A test body that reverts with the selector of `Unauthorized()`
const REVERT_UNAUTHORIZED: &str = "{ __ERROR(Unauthorized) 0x00 mstore 0x04 0x00 revert }";

#[test]
fn test_runner_fails_a_reverting_test_without_expectation() {
    let result = run_test_t("#define test T() = { 0x00 0x00 revert }");
    assert!(!result.passed);
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Revert));
    assert_eq!(result.outcome, Outcome::Revert { data: String::new() });
    assert_eq!(
        result.failures.iter().map(|f| (f.expectation.as_str(), f.message.as_str())).collect::<Vec<_>>(),
        vec![("outcome", "expected the test to return, but it reverted with 0x (empty)")]
    );
}

#[test]
fn test_runner_expect_revert_matches_any_revert() {
    let result = run_test_t(&with_error(&format!("#[expect_revert]\n#define test T() = {REVERT_UNAUTHORIZED}")));
    assert!(result.passed);
    assert_eq!(std::mem::discriminant(&result.status), std::mem::discriminant(&TestStatus::Success));
    assert_eq!(result.outcome, Outcome::Revert { data: String::from("82b42900") });
}

#[test]
fn test_runner_expect_revert_fails_when_the_test_returns() {
    assert_eq!(
        failures("#[expect_revert]\n#define test T() = { stop }"),
        vec![(String::from("expect_revert"), String::from("expected a revert, but the test returned 0x (empty)"))]
    );
}

#[test]
fn test_runner_expect_revert_checks_exact_data() {
    let source = with_error(&format!("#[expect_revert(__ERROR(Unauthorized))]\n#define test T() = {REVERT_UNAUTHORIZED}"));
    assert!(run_test_t(&source).passed);

    let source = with_error(&format!("#[expect_revert(__ERROR(Unauthorized), 0x01)]\n#define test T() = {REVERT_UNAUTHORIZED}"));
    assert_eq!(
        failures(&source),
        vec![(
            String::from("expect_revert"),
            String::from("expected revert data 0x82b4290001 (Unauthorized), got 0x82b42900 (Unauthorized)")
        )]
    );
}

#[test]
fn test_runner_expect_partial_revert_checks_a_prefix() {
    // Unauthorized() followed by an argument word still starts with its selector
    let test = "#define test T() = { __ERROR(Unauthorized) 0x00 mstore 0x2a 0x04 mstore 0x24 0x00 revert }";
    assert!(run_test_t(&with_error(&format!("#[expect_partial_revert(__ERROR(Unauthorized))]\n{test}"))).passed);
    assert_eq!(failures(&with_error(&format!("#[expect_partial_revert(0xdeadbeef)]\n{test}")))[0].0, "expect_partial_revert");
}

#[test]
fn test_runner_expect_halt_matches_reasons() {
    assert!(run_test_t("#[expect_halt]\n#define test T() = { invalid }").passed);
    assert!(run_test_t("#[expect_halt(\"InvalidFEOpcode\")]\n#define test T() = { invalid }").passed);
    assert!(run_test_t("#[expect_halt(\"InvalidJump\")]\n#define test T() = { 0x00 jump }").passed);
    // `OutOfGas` names every kind of out-of-gas halt
    assert!(run_test_t("#[gas_limit(30000), expect_halt(\"OutOfGas\")]\n#define test T() = { 0x01 0x00 sstore }").passed);

    let failures = failures("#[expect_halt(\"InvalidJump\")]\n#define test T() = { invalid }");
    assert_eq!(
        failures,
        vec![(String::from("expect_halt"), String::from("expected the test to halt with InvalidJump, but it halted with InvalidFEOpcode"))]
    );
}

#[test]
fn test_runner_expect_halt_does_not_match_a_revert() {
    assert_eq!(
        failures("#[expect_halt]\n#define test T() = { 0x00 0x00 revert }")[0].1,
        "expected the test to halt, but it reverted with 0x (empty)"
    );
    // and a revert expectation does not match a halt
    assert_eq!(
        failures("#[expect_revert]\n#define test T() = { invalid }")[0].1,
        "expected a revert, but the test halted with InvalidFEOpcode"
    );
}

#[test]
fn test_runner_expect_return_checks_exact_data() {
    assert!(run_test_t("#[expect_return(42)]\n#define test T() = { 0x2a 0x00 mstore 0x20 0x00 return }").passed);
    assert!(run_test_t("#[expect_return]\n#define test T() = { stop }").passed, "no arguments means empty return data");
    assert_eq!(
        failures("#[expect_return(0x01)]\n#define test T() = { stop }"),
        vec![(String::from("expect_return"), String::from("expected return data 0x01, got 0x (empty)"))]
    );
}

#[test]
fn test_runner_expect_gas_checks_bounds() {
    // PUSH0 + POP costs 4 gas
    assert!(run_test_t("#[expect_gas(min = 4, max = 4)]\n#define test T() = { push0 pop }").passed);
    assert_eq!(failures("#[expect_gas(max = 3)]\n#define test T() = { push0 pop }")[0].1, "used 4 gas, more than max 3");
    assert_eq!(failures("#[expect_gas(min = 5)]\n#define test T() = { push0 pop }")[0].1, "used 4 gas, less than min 5");
}

#[test]
fn test_runner_reports_every_failed_expectation() {
    let failures = failures("#[expect_revert, expect_gas(max = 1)]\n#define test T() = { push0 pop }");
    assert_eq!(failures.iter().map(|f| f.0.as_str()).collect::<Vec<_>>(), vec!["expect_revert", "expect_gas"]);
}

#[test]
fn test_runner_rejects_conflicting_outcome_expectations() {
    let err = decorator_error("#[expect_revert]\n#[expect_return(0x01)]\n#define test T() = { stop }");
    assert_eq!(err.flag, "expect_return");
    assert_eq!(err.kind, DecoratorErrorKind::Conflict);
    assert_eq!(err.span.0.len(), 2, "the error points at both flags");

    assert_eq!(decorator_error("#[expect_revert, expect_revert]\n#define test T() = { stop }").kind, DecoratorErrorKind::Duplicate);
}

#[test]
fn test_runner_rejects_wrong_expectation_arguments() {
    for (flag, kind) in [
        ("expect_gas", DecoratorErrorKind::InvalidArguments),
        ("expect_gas(100)", DecoratorErrorKind::InvalidArguments),
        ("expect_gas(min = 5, max = 4)", DecoratorErrorKind::InvalidValue),
        ("expect_partial_revert", DecoratorErrorKind::InvalidArguments),
        ("expect_halt(0x01)", DecoratorErrorKind::InvalidValue),
        ("expect_halt(\"OutOfGas\", \"InvalidJump\")", DecoratorErrorKind::InvalidArguments),
    ] {
        assert_eq!(decorator_error(&format!("#[{flag}]\n#define test T() = {{ stop }}")).kind, kind, "{flag}");
    }
}

/// A test body that calls the Foundry cheatcode with this signature and one word argument
fn call_cheatcode(signature: &str, arg: &str) -> String {
    format!(
        "__FUNC_SIG(\"{signature}\") 0xe0 shl 0x00 mstore {arg} 0x04 mstore \
         0x00 0x00 0x24 0x00 0x00 0x7109709ECfa91a80626fF3989D68f67F5b1DD12D gas call pop"
    )
}

#[test]
fn test_runner_serves_cheatcodes() {
    let roll = call_cheatcode("roll(uint256)", "0x2a");
    let result = run_test_t(&format!("#[expect_return(42)]\n#define test T() = {{ {roll} number 0x00 mstore 0x20 0x00 return }}"));
    assert!(result.passed, "{:?}", result.failures);
}

#[test]
fn test_runner_fails_a_test_with_a_failed_cheatcode_assertion() {
    // A failed assertion does not revert, so only Foundry's failure flag reveals it
    let assert_false = call_cheatcode("assertTrue(bool)", "0x00");
    let result = run_test_t(&format!("#define test T() = {{ {assert_false} stop }}"));
    assert!(!result.passed);
    assert_eq!(result.outcome, Outcome::Return { data: String::new() });
    assert_eq!(result.failures[0].expectation, "vm.assert");

    let assert_true = call_cheatcode("assertTrue(bool)", "0x01");
    assert!(run_test_t(&format!("#define test T() = {{ {assert_true} stop }}")).passed);
}

#[test]
fn test_runner_applies_deal_decorator() {
    let source = r#"
    #define constant AMOUNT = 0x0100
    #[deal(this, 7), deal(0x1000, [AMOUNT]), deal(caller, 9)]
    #define test T() = {
        selfbalance 0x00 mstore
        0x1000 balance 0x20 mstore
        caller balance 0x40 mstore
        0x60 0x00 return
    }
    "#;

    let result = run_test_t(source);
    assert!(result.passed, "{:?}", result.failures);
    // A dealt sender keeps its balance instead of the runner's funding, as the test pays no gas
    assert_eq!(result.return_data, Some(format!("{:064x}{:064x}{:064x}", 7, 0x100, 9)));
}

#[test]
fn test_runner_applies_etch_decorator() {
    // The etched code returns the word 0x2a
    let code = "602a5f5260205ff3";
    let source = format!(
        r#"
    #[etch(0x1000, "0x{code}")]
    #define test T() = {{
        0x20 0x00 0x00 0x00 0x1000 gas staticcall pop
        0x1000 extcodehash 0x20 mstore
        0x40 0x00 return
    }}
    "#
    );

    let result = run_test_t(&source);
    assert!(result.passed, "{:?}", result.failures);
    let code_hash = keccak256(hex::decode(code).unwrap());
    assert_eq!(result.return_data, Some(format!("{:064x}{}", 0x2a, hex::encode(code_hash))));
}

#[test]
fn test_runner_applies_nonce_decorator() {
    let source = "#[nonce(0x1000, 5), nonce(caller, 7)]\n#define test T() = { stop }";

    let mut executor = new_executor();
    let result = run_source_test_in(&mut executor, source, "T").unwrap();
    assert!(result.passed, "{:?}", result.failures);
    assert_eq!(executor.backend().basic_ref(OTHER).unwrap().unwrap().nonce, 5);
    // Like Foundry, the test call itself does not count as a transaction of the sender
    assert_eq!(executor.backend().basic_ref(TestRunner::default().env.tx.caller).unwrap().unwrap().nonce, 7);
}

#[test]
fn test_runner_fresh_dealt_account_has_empty_code_hash() {
    let source = "#[deal(0x1000, 1)]\n#define test T() = { 0x1000 extcodehash 0x00 mstore 0x20 0x00 return }";

    let result = run_test_t(source);
    assert_eq!(result.return_data, Some(hex::encode(KECCAK256_EMPTY)));
}

#[test]
fn test_runner_expect_storage_and_balance_pass() {
    let source = r#"
    #define constant SLOT = FREE_STORAGE_POINTER()
    #define macro MAIN() = { [SLOT] sload }
    #[value(5)]
    #[expect_storage([SLOT], 0x2a), expect_storage(0x01, 0x07, at = 0x1000), expect_balance(this, 5)]
    #[store(0x01, 0x07, at = 0x1000)]
    #define test T() = { 0x2a [SLOT] sstore }
    "#;

    let result = run_test_t(source);
    assert!(result.passed, "{:?}", result.failures);
}

#[test]
fn test_runner_expect_storage_and_balance_report_mismatches() {
    let source = "#[expect_storage(0x00, 0x05), expect_balance(0x1000, 3)]\n#define test T() = { 0x04 0x00 sstore }";

    let this = run_test_t(source).address.unwrap();
    assert_eq!(
        failures(source),
        vec![
            (String::from("expect_storage"), format!("slot 0x0 at {this}: expected 0x5, got 0x4")),
            (String::from("expect_balance"), format!("balance of {OTHER}: expected 3, got 0")),
        ]
    );
}

#[test]
fn test_runner_skips_post_state_checks_when_the_test_does_not_return() {
    // The revert rolled the state back, so only the outcome is reported
    let failures = failures("#[expect_storage(0x00, 0x01)]\n#define test T() = { 0x01 0x00 sstore 0x00 0x00 revert }");
    assert_eq!(failures.iter().map(|f| f.0.as_str()).collect::<Vec<_>>(), vec!["outcome"]);
}

#[test]
fn test_runner_rejects_post_state_checks_on_a_reverting_test() {
    for outcome in ["expect_revert", "expect_partial_revert(0x01)", "expect_halt"] {
        let err = decorator_error(&format!("#[expect_balance(this, 0)]\n#[{outcome}]\n#define test T() = {{ stop }}"));
        assert_eq!(err.flag, "expect_balance", "{outcome}");
        assert_eq!(err.kind, DecoratorErrorKind::Conflict, "{outcome}");
        assert_eq!(err.span.0.len(), 2, "the error points at both flags");
    }
    // Returning keeps the state, so it can be checked
    assert!(run_test_t("#[expect_return, expect_storage(0x00, 0x00)]\n#define test T() = { stop }").passed);
}

#[test]
fn test_runner_rejects_pre_and_post_state_on_the_same_account_twice() {
    for flags in [
        "deal(0x1000, 1), deal(4096, 2)",
        "etch(0x1000, 0x00), etch(0x1000, 0x01)",
        "nonce(0x1000, 1), nonce(0x1000, 2)",
        "expect_balance(this, 1), expect_balance(this, 2)",
        "expect_storage(0x01, 0x01), expect_storage(1, 0x02, at = this)",
    ] {
        let err = decorator_error(&format!("#[{flags}]\n#define test T() = {{ stop }}"));
        assert_eq!(err.kind, DecoratorErrorKind::Duplicate, "{flags}");
        assert_eq!(err.span.0.len(), 2, "the error points at both flags");
    }
    // Different flags may name the same address
    assert!(
        run_test_t("#[deal(0x1000, 1), nonce(0x1000, 1), etch(0x1000, 0x00), expect_balance(0x1000, 1)]\n#define test T() = { stop }")
            .passed
    );
}

#[test]
fn test_runner_rejects_etching_the_test_or_its_sender() {
    for target in ["this", "caller"] {
        let err = decorator_error(&format!("#[etch({target}, 0x00)]\n#define test T() = {{ stop }}"));
        assert_eq!(err.kind, DecoratorErrorKind::InvalidValue, "{target}");
    }
    let err = decorator_error("#[caller(0xbeef), etch(0xbeef, 0x00)]\n#define test T() = { stop }");
    assert_eq!(err.kind, DecoratorErrorKind::InvalidValue);
}

#[test]
fn test_runner_rejects_wrong_pre_and_post_state_arguments() {
    for (flag, kind) in [
        ("deal(0x1000)", DecoratorErrorKind::InvalidArguments),
        ("deal(0x1000, 1, 2)", DecoratorErrorKind::InvalidArguments),
        ("etch(0x1000)", DecoratorErrorKind::InvalidArguments),
        ("nonce(0x1000, 0x010000000000000000)", DecoratorErrorKind::InvalidValue),
        ("expect_storage(0x01)", DecoratorErrorKind::InvalidArguments),
        ("expect_storage(0x01, 0x02, to = 0x03)", DecoratorErrorKind::InvalidArguments),
        ("expect_balance(this)", DecoratorErrorKind::InvalidArguments),
        ("expect_balance(0x010000000000000000000000000000000000000000, 1)", DecoratorErrorKind::InvalidValue),
        ("expect_balance(this, 1, min = 0)", DecoratorErrorKind::InvalidArguments),
        ("expect_balance(this, gt = 1)", DecoratorErrorKind::InvalidArguments),
        ("expect_balance(this, min = 6, max = 5)", DecoratorErrorKind::InvalidValue),
        ("expect_storage(0x01, 0x02, max = 3)", DecoratorErrorKind::InvalidArguments),
        ("expect_storage(0x01, at = 0x1000)", DecoratorErrorKind::InvalidArguments),
        ("expect_storage(0x01, min = 3, max = 2)", DecoratorErrorKind::InvalidValue),
    ] {
        assert_eq!(decorator_error(&format!("#[{flag}]\n#define test T() = {{ stop }}")).kind, kind, "{flag}");
    }
}

#[test]
fn test_runner_expect_balance_and_storage_check_inclusive_bounds() {
    // The test holds 5 wei and stores 5 in slot 0 after the call
    let test = "#define test T() = { 0x05 0x00 sstore }";
    for (bounds, passes) in [
        ("min = 5", true),
        ("min = 6", false),
        ("max = 5", true),
        ("max = 4", false),
        ("min = 5, max = 5", true),
        ("min = 0, max = 4", false),
    ] {
        let result = run_test_t(&format!("#[value(5), expect_balance(this, {bounds}), expect_storage(0x00, {bounds})]\n{test}"));
        assert_eq!(result.passed, passes, "{bounds}: {:?}", result.failures);
        assert_eq!(result.failures.len(), if passes { 0 } else { 2 }, "{bounds}");
    }

    let failures = failures(&format!("#[value(5), expect_balance(this, min = 6, max = 9), expect_storage(0x00, max = 4)]\n{test}"));
    assert!(failures[0].1.ends_with("expected at most 0x4, got 0x5"), "{failures:?}");
    assert!(failures[1].1.ends_with("expected at least 6 and at most 9, got 5"), "{failures:?}");
}

#[test]
fn test_runner_expectation_bounds_accept_hex_decimal_and_constants() {
    let source = r#"
    #define constant CAP = 0x0a
    #[value(5), store(0x00, 7)]
    #[expect_balance(this, min = 0x05, max = [CAP]), expect_storage(0x00, min = 7, max = [CAP])]
    #define test T() = { stop }
    "#;

    let result = run_test_t(source);
    assert!(result.passed, "{:?}", result.failures);
}

#[test]
fn test_runner_resolves_storage_pointers_used_only_by_tests() {
    // No MAIN uses the pointer: the test reaches it through a macro and a decorator
    let source = r#"
    #define constant COUNTER_SLOT = FREE_STORAGE_POINTER()
    #define macro INCREMENT() = { [COUNTER_SLOT] sload 0x01 add [COUNTER_SLOT] sstore }
    #[store([COUNTER_SLOT], 0x01), expect_storage([COUNTER_SLOT], 0x02)]
    #define test T() = { INCREMENT() }
    "#;

    let result = run_test_t(source);
    assert!(result.passed, "{:?}", result.failures);
}

#[test]
fn test_runner_test_calling_a_fn_ends_without_entering_it() {
    // The functions are appended after the test, which ends by running off its code
    let source = r#"
    #define fn INCREMENT() = takes (1) returns (1) { 0x01 add }
    #[expect_storage(0x00, 0x03)]
    #define test T() = { 0x02 INCREMENT() 0x00 sstore }
    "#;

    let result = run_test_t(source);
    assert!(result.passed, "{:?}", result.failures);
}
