//! Deploys a constructor larger than 64 KiB, which Amsterdam allows (EIP-7954) and which needs
//! PUSH3 jump targets.

use alloy_primitives::{U256, hex};
use foundry_evm::backend::DatabaseError;
use huff_neo_codegen::Codegen;
use huff_neo_lexer::Lexer;
use huff_neo_parser::Parser;
use huff_neo_test_runner::prelude::TestRunner;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::{EVMVersion, SupportedEVMVersions, Token};
use revm::context::result::{ExecutionResult, Output};
use revm::context::{TransactTo, TxEnv};
use revm::database::{CacheDB, EmptyDBTyped};
use revm::primitives::hardfork::SpecId;
use revm::{Context, ExecuteCommitEvm, MainBuilder, MainContext};
use std::sync::Arc;

/// The constructor jumps forward and backward across 64 KiB of INVALID opcodes, also from inside a
/// nested macro. Any wrong jump target hits an INVALID opcode or a non-JUMPDEST and aborts.
const LARGE_CONSTRUCTOR: &str = r#"
    #define macro SKIP() = takes(0) returns(0) {
        skip jump
        invalid
        skip:
    }

    #define macro CONSTRUCTOR() = takes(0) returns(0) {
        forward jump
        back:
            SKIP()
            done jump
        for(i in 0..65536) { invalid }
        forward:
            SKIP()
            back jump
        done:
    }

    #define macro MAIN() = takes(0) returns(0) {
        0x2a 0x00 mstore
        0x20 0x00 return
    }
"#;

/// Compiles the deployment bytecode and the runtime for Amsterdam.
fn compile_amsterdam(source: &str) -> (String, String) {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let tokens = Lexer::new(flattened_source).map(|x| x.unwrap()).collect::<Vec<Token>>();
    let mut contract = Parser::new(tokens, None).parse().unwrap();
    contract.derive_storage_pointers();

    let evm = &EVMVersion::new(SupportedEVMVersions::Amsterdam);
    let main = Codegen::generate_main_bytecode_with_sourcemap(evm, &contract, None, false).unwrap();
    contract.runtime_size = Some(main.bytecode.len() / 2);
    let (constructor, contract) = Codegen::generate_constructor_macro_bytecode(evm, &contract, None, false).unwrap();
    let artifact =
        Codegen::new().assemble_artifact(evm, Arc::new(FileSource::default()), &contract, vec![], main, Some(constructor), false).unwrap();
    (artifact.bytecode, artifact.runtime)
}

/// Sends a contract creation transaction with the given initcode.
fn deploy(spec: SpecId, initcode: &str) -> Result<ExecutionResult, String> {
    let mut db = CacheDB::<EmptyDBTyped<DatabaseError>>::default();
    let mut runner = TestRunner::default();
    runner.env.evm_env.cfg_env.set_spec_and_mainnet_gas_params(spec);
    runner.env.evm_env.cfg_env.disable_base_fee = true;
    runner.set_balance(&mut db, runner.env.tx.caller, U256::MAX).unwrap();

    let tx = TxEnv {
        caller: runner.env.tx.caller,
        kind: TransactTo::Create,
        data: hex::decode(initcode).unwrap().into(),
        gas_limit: 16_000_000,
        chain_id: Some(runner.env.evm_env.cfg_env.chain_id),
        ..Default::default()
    };
    let mut evm = Context::mainnet().with_cfg(runner.env.evm_env.cfg_env.clone()).with_db(&mut db).build_mainnet();
    evm.transact_commit(tx).map_err(|e| format!("{e:?}"))
}

#[test]
fn test_deploy_constructor_beyond_64k() {
    let (initcode, runtime) = compile_amsterdam(LARGE_CONSTRUCTOR);
    assert!(initcode.len() / 2 > 0x10000, "the constructor must be larger than 64 KiB");
    assert!(initcode.starts_with("62") && &initcode[8..10] == "56", "the first jump must use PUSH3: {}", &initcode[..10]);

    match deploy(SpecId::AMSTERDAM, &initcode).unwrap() {
        ExecutionResult::Success { output: Output::Create(code, Some(_)), .. } => {
            assert_eq!(hex::encode(code), runtime, "the deployed code must be the runtime");
        }
        other => panic!("deployment failed: {other:?}"),
    }
}

#[test]
fn test_constructor_beyond_64k_exceeds_pre_amsterdam_initcode_limit() {
    // Before Amsterdam, initcode is limited to 48 KiB (EIP-3860), so the same deployment fails
    let (initcode, _) = compile_amsterdam(LARGE_CONSTRUCTOR);
    let error = deploy(SpecId::OSAKA, &initcode).unwrap_err();
    assert!(error.contains("CreateInitCodeSizeLimit"), "unexpected error: {error}");
}
