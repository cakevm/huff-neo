#![doc = include_str!("../README.md")]
#![allow(dead_code)]
#![allow(clippy::enum_variant_names)]
#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]
#![allow(deprecated)]

mod arguments;

use crate::arguments::{get_input, HuffArgs, TestCommands};
use alloy_primitives::hex;
use clap::{CommandFactory, Parser};
use comfy_table::{modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Cell, Color, Row, Table};
use foundry_cli::utils::LoadConfig;
use foundry_evm_traces::InternalTraceMode;
use huff_neo_codegen::Codegen;
use huff_neo_core::Compiler;
use huff_neo_test_runner::{
    prelude::{print_test_report, ReportKind},
    HuffTester, HuffTesterConfig, Inspector,
};
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::bytecode::Bytes;
use huff_neo_utils::file::file_provider::FileSystemFileProvider;
use huff_neo_utils::file::file_source::FileSource;
use huff_neo_utils::file::full_file_source::OutputLocation;
use huff_neo_utils::prelude::{
    export_interfaces, gen_sol_interfaces, BytecodeRes, CodegenError, CodegenErrorKind, CompilerError, EVMVersion, Span,
};
use shadow_rs::shadow;
use std::process::exit;
use std::{collections::BTreeMap, rc::Rc, sync::Arc, time::Instant};
use yansi::Paint;

shadow!(build);

fn main() {
    let mut command = HuffArgs::command();

    // Parse the command line arguments
    let mut cli = HuffArgs::parse();

    if cli.version_long {
        let git_clean = if build::GIT_CLEAN { "" } else { "-dirty" };
        println!("hnc {} (rev: {}{})", build::PKG_VERSION, build::SHORT_COMMIT, git_clean);
        return;
    }

    // Initiate Tracing if Verbose
    if cli.verbose {
        Compiler::init_tracing_subscriber(Some(vec![tracing::Level::DEBUG.into()]));
    }

    // Check if no argument is provided
    if cli.path.is_none() {
        println!("{}", Paint::red("No input file provided"));
        // Print help and exit
        command.print_help().unwrap();
        return;
    }

    // Create compiler from the Huff Args
    let sources: Arc<Vec<String>> = match cli.get_inputs() {
        Ok(s) => Arc::new(s),
        Err(e) => {
            eprintln!("{}", Paint::red(&format!("{e}")));
            exit(1);
        }
    };

    // If constant overrides were passed, create a map of their names and values
    let constants: Option<BTreeMap<&str, Bytes>> = cli.constants.as_ref().map(|_constants| {
        _constants
            .iter()
            .map(|c: &String| {
                let parts = c.as_str().split('=').collect::<Vec<_>>();

                // Check that constant override argument is valid
                // Key rule: Alphabetic chars + underscore
                // Value rule: Valid literal string (0x...)
                if parts.len() != 2
                    || parts[0].chars().any(|c| !(c.is_alphabetic() || c == '_'))
                    || !parts[1].starts_with("0x")
                    || parts[1][2..].chars().any(|c| !(c.is_numeric() || matches!(c, '\u{0041}'..='\u{0046}' | '\u{0061}'..='\u{0066}')))
                {
                    eprintln!("Invalid constant override argument: {}", Paint::red(&c.to_string()));
                    exit(1);
                }

                (parts[0], Bytes(parts[1][2..].to_string()))
            })
            .collect()
    });

    // Parse the EVM version
    let evm_version = EVMVersion::from(cli.evm_version);

    let mut use_cache = true;
    if cli.interactive {
        // Don't accept configured inputs
        cli.inputs = None;
        // Don't export artifacts are compile
        // Have to first generate artifacts, prompt user for args,
        // and finally save artifacts with the new constructor args.
        cli.artifacts = false;
        // Don't use cache if interactive since there's no way constructor arguments can match
        use_cache = false;
    }

    let output = match (&cli.output, cli.artifacts) {
        (Some(o), true) => Some(o.clone()),
        (None, true) => Some(cli.outputdir.clone()),
        _ => None,
    };

    let compiler: Compiler = Compiler {
        evm_version: &evm_version,
        sources: Arc::clone(&sources),
        output,
        alternative_main: cli.alternative_main.clone(),
        alternative_constructor: cli.alternative_constructor,
        construct_args: cli.inputs,
        constant_overrides: constants,
        optimize: cli.optimize,
        bytecode: cli.bytecode,
        cached: use_cache,
        file_provider: Arc::new(FileSystemFileProvider {}),
    };

    if cli.label_indices {
        let contracts = match compiler.grab_contracts() {
            Ok(contracts) => contracts,
            Err(e) => {
                tracing::error!(target: "cli", "PARSER ERRORED!");
                eprintln!("{}", Paint::red(&e));
                exit(1);
            }
        };

        if contracts.len() > 1 {
            eprintln!("{}", Paint::red("Multiple contracts found. Please specify a single contract and try again."));
            exit(1);
        }

        if let Some(contract) = contracts.first() {
            let macro_def = contract.find_macro_by_name(&cli.alternative_main.unwrap_or_else(|| "MAIN".to_string())).unwrap_or_else(|| {
                eprintln!("{}", Paint::red("Macro not found. Please specify a valid macro and try again."));
                exit(1);
            });

            // Recurse through the macro and generate bytecode
            let bytecode_res: BytecodeRes =
                Codegen::macro_to_bytecode(&evm_version, macro_def, contract, &mut vec![macro_def], 0, &mut Vec::default(), false, None)
                    .unwrap();

            if bytecode_res.label_indices.is_empty() {
                eprintln!(
                    "{}",
                    Paint::red(
                        "No jump labels found. Please try again.\nHint: you can run this command on a specific macro by adding the `-m <macro_name>` flag.\n"
                    )
                );
                exit(1);
            }

            // Format the label indices nicely in a table
            let mut table = Table::new();
            table.load_preset(UTF8_FULL).apply_modifier(UTF8_ROUND_CORNERS);
            table
                .set_header(vec![Cell::new("Jump Label").fg(Color::Cyan), Cell::new("Program counter offset (in hex)").fg(Color::Cyan)])
                .add_rows(
                    bytecode_res
                        .label_indices
                        .iter()
                        .map(|(label, index)| Row::from(vec![Cell::new(label), Cell::new(format!("{:#04x}", index))])),
                );
            println!("{table}");
        } else {
            eprintln!("{}", Paint::red("No contract found. Please specify a contract and try again."));
            exit(1);
        }

        return;
    }

    if let Some(TestCommands::Test(test_args)) = cli.test {
        let contracts = match compiler.grab_contracts() {
            Ok(contracts) => contracts,
            Err(e) => {
                tracing::error!(target: "cli", "PARSER ERRORED!");
                eprintln!("{}", Paint::red(&e));
                exit(1);
            }
        };

        // Merge all configs.
        let (mut config, mut evm_opts) = test_args.load_config_and_evm_opts().unwrap();

        // Explicitly enable isolation for gas reports for more correct gas accounting.
        if test_args.gas_report {
            evm_opts.isolate = true;
        } else {
            // Do not collect gas report traces if gas report is not enabled.
            config.fuzz.gas_report_samples = 0;
            config.invariant.gas_report_samples = 0;
        }

        let match_ = Rc::new(test_args.match_);
        let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build().unwrap();

        for contract in &contracts {
            let mut inspectors = Inspector::default();
            if test_args.verbosity > 2 {
                inspectors = inspectors.with_log_collector();
            }
            if test_args.verbosity > 3 {
                inspectors = inspectors.with_steps_tracing();
            }

            let mut env = rt.block_on(evm_opts.evm_env()).unwrap();

            // Set the sender address if provided.
            if let Some(sender) = test_args.evm.sender {
                env.tx.caller = sender;
            }

            // Disable base fee because simulation would fail
            env.cfg.disable_base_fee = true;

            // Choose the internal function tracing mode, if --decode-internal is provided.
            let decode_internal = if test_args.decode_internal {
                // If more than one function matched, we enable simple tracing.
                // If only one function matched, we enable full tracing. This is done in `run_tests`.
                if contracts.len() == 1 {
                    InternalTraceMode::Full
                } else {
                    InternalTraceMode::Simple
                }
            } else {
                InternalTraceMode::None
            };

            let tester_config = HuffTesterConfig::new()
                .set_debug(test_args.debug)
                .set_decode_internal(decode_internal)
                .initial_balance(evm_opts.initial_balance)
                .evm_spec(config.evm_spec_id())
                .sender(evm_opts.sender)
                .with_fork(evm_opts.get_fork(&config, env.clone()))
                .enable_isolation(evm_opts.isolate)
                .odyssey(evm_opts.odyssey)
                .target_address(test_args.target_address);

            let tester = HuffTester::new(contract, Rc::clone(&match_), inspectors, tester_config, env);

            let _guard = rt.enter();
            let start = Instant::now();
            match tester.execute() {
                Ok(res) => {
                    rt.block_on(async {
                        match print_test_report(res, ReportKind::from(&test_args.format), start).await {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("{}", Paint::red(&format!("{e}")));
                                exit(1);
                            }
                        }
                    });
                }
                Err(e) => {
                    eprintln!("{}", Paint::red(&e));
                    exit(1);
                }
            };
        }

        return;
    }

    let mut artifacts = match compiler.execute() {
        Ok(artifacts) => artifacts,
        Err(e) => {
            tracing::error!(target: "cli", "COMPILER ERRORED: {}", e);
            eprintln!("{}", Paint::red(&format!("{e}")));
            exit(1);
        }
    };

    if artifacts.is_empty() {
        let e = CompilerError::CodegenError(CodegenError {
            kind: CodegenErrorKind::AbiGenerationFailure,
            span: AstSpan(
                sources
                    .iter()
                    .map(|s| Span {
                        start: 0,
                        end: 0,
                        file: Some(Arc::new(FileSource {
                            id: uuid::Uuid::new_v4(),
                            path: s.clone(),
                            source: None,
                            access: None,
                            dependencies: vec![],
                        })),
                    })
                    .collect::<Vec<Span>>(),
            ),
            token: None,
        });
        tracing::error!(target: "cli", "COMPILER ERRORED: {}", e);
        eprintln!("{}", Paint::red(&format!("{e}")));
        exit(1);
    }

    if command.get_matches().contains_id("interface") {
        let mut interface: Option<String> = None;
        if artifacts.len() == 1 {
            let gen_interface: Option<String> = match artifacts[0].file.path.split('/').last() {
                Some(p) => match p.split('.').next() {
                    Some(p) => Some(format!("I{p}")),
                    None => {
                        tracing::warn!(target: "cli", "No file name found for artifact");
                        None
                    }
                },
                None => {
                    tracing::warn!(target: "cli", "No trailing string");
                    None
                }
            };
            interface = Some(cli.interface.unwrap_or_else(|| gen_interface.unwrap_or_else(|| "Interface".to_string())));
        } else if cli.interface.is_some() {
            tracing::warn!(target: "cli", "Interface override ignored since multiple artifacts were generated");
        }
        tracing::info!(target: "cli", "GENERATING SOLIDITY INTERFACES FROM ARTIFACTS");
        let interfaces = gen_sol_interfaces(&artifacts, interface);
        if export_interfaces(&interfaces).is_ok() {
            tracing::info!(target: "cli", "GENERATED SOLIDITY INTERFACES FROM ARTIFACTS SUCCESSFULLY");
            println!(
                "Exported Solidity Interfaces: {}",
                Paint::blue(&interfaces.into_iter().map(|(_, i, _)| format!("{i}.sol")).collect::<Vec<_>>().join(", "))
            );
        } else {
            tracing::error!(target: "cli", "FAILED TO GENERATE SOLIDITY INTERFACES FROM ARTIFACTS");
            eprintln!("{}", Paint::red("FAILED TO GENERATE SOLIDITY INTERFACES FROM ARTIFACTS"));
        }
    }

    if cli.bytecode {
        if cli.interactive {
            tracing::info!(target: "cli", "ENTERING INTERACTIVE MODE");
            // let mut new_artifacts = vec![];
            for artifact in &mut artifacts {
                let mut appended_args = String::default();
                match artifact.abi {
                    Some(ref abi) => match abi.constructor {
                        Some(ref args) => {
                            println!(
                                "{} Constructor Arguments for Contract: \"{}\"",
                                Paint::blue(&"[INTERACTIVE]".to_string()),
                                artifact.file.path
                            );
                            for input in &args.inputs {
                                let arg_input = get_input(&format!(
                                    "Enter a {:?} for constructor param{}:",
                                    input.kind,
                                    (!input.name.is_empty()).then(|| format!(" \"{}\"", input.name)).unwrap_or_default()
                                ));
                                let encoded =
                                    Codegen::encode_constructor_args(vec![arg_input]).iter().fold(String::default(), |acc, str| {
                                        let inner: Vec<u8> = str.abi_encode();
                                        let hex_args: String = hex::encode(inner.as_slice());
                                        format!("{acc}{hex_args}")
                                    });
                                appended_args.push_str(&encoded);
                            }
                        }
                        None => {
                            tracing::warn!(target: "cli", "NO CONSTRUCTOR FOR ABI: {:?}", abi)
                        }
                    },
                    None => {
                        tracing::warn!(target: "cli", "NO ABI FOR ARTIFACT: {:?}", artifact)
                    }
                }
                match Arc::get_mut(artifact) {
                    Some(art) => {
                        art.bytecode = format!("{}{appended_args}", art.bytecode);
                    }
                    None => {
                        tracing::warn!(target: "cli", "FAILED TO ACQUIRE MUTABLE REF TO ARTIFACT")
                    }
                }
            }
            tracing::debug!(target: "cli", "Re-exporting artifacts...");
            Compiler::export_artifacts(&artifacts, &OutputLocation(cli.output.unwrap_or_else(|| cli.outputdir.clone())));
            tracing::info!(target: "cli", "RE-EXPORTED INTERACTIVE ARTIFACTS");
        }
        match sources.len() {
            1 => {
                if cli.bin_runtime {
                    println!("\nbytecode: {}", artifacts[0].bytecode)
                } else {
                    print!("{}", artifacts[0].bytecode)
                }
            }
            _ => artifacts.iter().for_each(|a| println!("\"{}\" bytecode: {}", a.file.path, a.bytecode)),
        }
    }

    if cli.bin_runtime {
        match sources.len() {
            1 => {
                if cli.bytecode {
                    println!("\nruntime: {}", artifacts[0].runtime)
                } else {
                    print!("{}", artifacts[0].runtime)
                }
            }
            _ => artifacts.iter().for_each(|a| println!("\"{}\" runtime: {}", a.file.path, a.runtime)),
        }
    }
}
