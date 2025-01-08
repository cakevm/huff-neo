#![doc = include_str!("../README.md")]
#![allow(dead_code)]
#![allow(clippy::enum_variant_names)]
#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]
#![allow(deprecated)]

use alloy_primitives::hex;
use clap::{CommandFactory, Parser as ClapParser, Subcommand};
use comfy_table::{modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Cell, Color, Row, Table};
use huff_neo_codegen::Codegen;
use huff_neo_core::Compiler;
use huff_neo_tests::{
    prelude::{print_test_report, ReportKind},
    HuffTester,
};
use huff_neo_utils::{
    file_provider::FileSystemFileProvider,
    prelude::{
        export_interfaces, gen_sol_interfaces, str_to_bytes32, unpack_files, AstSpan, BytecodeRes, CodegenError, CodegenErrorKind,
        CompilerError, EVMVersion, FileSource, Literal, OutputLocation, Span,
    },
};
use std::{collections::BTreeMap, io::Write, path::Path, rc::Rc, sync::Arc, time::Instant};
use yansi::Paint;

/// The Huff CLI Args
#[derive(ClapParser, Debug, Clone)]
#[clap(name = "huff-neo", version, about, long_about = None)]
struct Huff {
    /// The contract(s) to compile.
    pub path: Option<String>,

    /// The contracts source path.
    #[clap(short = 's', long = "source-path", default_value = "./contracts")]
    source: String,

    /// The output file path.
    #[clap(short = 'o', long = "output")]
    output: Option<String>,

    /// The output directory.
    #[clap(short = 'd', long = "output-directory", default_value = "./artifacts")]
    outputdir: String,

    /// The input constructor arguments
    #[clap(short = 'i', long = "inputs", num_args = 1..)]
    inputs: Option<Vec<String>>,

    /// Interactively input the constructor args
    #[clap(short = 'n', long = "interactive")]
    interactive: bool,

    /// Whether to generate artifacts or not
    #[clap(short = 'a', long = "artifacts")]
    artifacts: bool,

    /// Optimize compilation [WIP]
    #[clap(short = 'z', long = "optimize")]
    optimize: bool,

    /// Generate solidity interface for a Huff artifact
    #[clap(short = 'g', num_args = 0.., long = "interface")]
    interface: Option<String>,

    /// Generate and log bytecode.
    #[clap(short = 'b', long = "bytecode")]
    bytecode: bool,

    /// Generate and log runtime bytecode.
    #[clap(short = 'r', long = "bin-runtime")]
    bin_runtime: bool,

    /// Prints out to the terminal.
    #[clap(short = 'p', long = "print")]
    print: bool,

    /// Verbose output.
    #[clap(short = 'v', long = "verbose")]
    verbose: bool,

    /// Prints out the jump label PC indices for the specified contract.
    #[clap(short = 'l', long = "label-indices")]
    label_indices: bool,

    /// Override / set constants for the compilation environment.
    #[clap(short = 'c', long = "constants", num_args = 1..)]
    constants: Option<Vec<String>>,

    /// Compile a specific macro
    #[clap(short = 'm', long = "alt-main")]
    alternative_main: Option<String>,

    /// Compile a specific constructor macro
    #[clap(short = 't', long = "alt-constructor")]
    alternative_constructor: Option<String>,

    /// Set the EVM version
    #[clap(short = 'e', long = "evm-version")]
    evm_version: Option<String>,

    /// Test subcommand
    #[clap(subcommand)]
    test: Option<TestCommands>,
}

#[derive(Subcommand, Clone, Debug)]
enum TestCommands {
    /// Test subcommand
    Test {
        /// Format the test output as a list, table, or JSON.
        #[clap(short = 'f', long = "format")]
        format: Option<String>,

        /// Match a specific test
        #[clap(short = 'm', long = "match")]
        match_: Option<String>,
    },
}

/// Helper function to read a stdin input
pub(crate) fn get_input(prompt: &str) -> String {
    print!("{} {prompt} ", Paint::blue(&"[INTERACTIVE]".to_string()));
    let mut input = String::new();
    let _ = std::io::stdout().flush();
    let _ = std::io::stdin().read_line(&mut input);
    // sp.stop();
    input.trim().to_string()
}

fn main() {
    let mut command = Huff::command();

    // Parse the command line arguments
    let mut cli = Huff::parse();

    // Initiate Tracing if Verbose
    if cli.verbose {
        Compiler::init_tracing_subscriber(Some(vec![tracing::Level::DEBUG.into()]));
    }

    // Check if no argument is provided
    if cli.path.is_none() {
        // Print help and exit
        command.print_help().unwrap();
        return;
    }

    // Create compiler from the Huff Args
    let sources: Arc<Vec<String>> = match cli.get_inputs() {
        Ok(s) => Arc::new(s),
        Err(e) => {
            eprintln!("{}", Paint::red(&format!("{e}")));
            std::process::exit(1);
        }
    };

    // If constant overrides were passed, create a map of their names and values
    let constants: Option<BTreeMap<&str, Literal>> = cli.constants.as_ref().map(|_constants| {
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
                    std::process::exit(1);
                }

                (parts[0], str_to_bytes32(&parts[1][2..]))
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
        match compiler.grab_contracts() {
            Ok(contracts) => {
                if contracts.len() > 1 {
                    eprintln!("{}", Paint::red("Multiple contracts found. Please specify a single contract and try again."));
                    std::process::exit(1);
                }

                if let Some(contract) = contracts.first() {
                    let macro_def =
                        contract.find_macro_by_name(&cli.alternative_main.unwrap_or_else(|| "MAIN".to_string())).unwrap_or_else(|| {
                            eprintln!("{}", Paint::red("Macro not found. Please specify a valid macro and try again."));
                            std::process::exit(1);
                        });

                    // Recurse through the macro and generate bytecode
                    let bytecode_res: BytecodeRes = Codegen::macro_to_bytecode(
                        &evm_version,
                        macro_def,
                        contract,
                        &mut vec![macro_def],
                        0,
                        &mut Vec::default(),
                        false,
                        None,
                    )
                    .unwrap();

                    if !bytecode_res.label_indices.is_empty() {
                        // Format the label indices nicely in a table
                        let mut table = Table::new();
                        table.load_preset(UTF8_FULL).apply_modifier(UTF8_ROUND_CORNERS);
                        table
                            .set_header(vec![
                                Cell::new("Jump Label").fg(Color::Cyan),
                                Cell::new("Program counter offset (in hex)").fg(Color::Cyan),
                            ])
                            .add_rows(
                                bytecode_res
                                    .label_indices
                                    .iter()
                                    .map(|(label, index)| Row::from(vec![Cell::new(label), Cell::new(format!("{:#04x}", index))])),
                            );
                        println!("{table}");
                    } else {
                        eprintln!(
                            "{}",
                            Paint::red(
                                "No jump labels found. Please try again.\nHint: you can run this command on a specific macro by adding the `-m <macro_name>` flag.\n"
                            )
                        );
                        std::process::exit(1);
                    }
                } else {
                    eprintln!("{}", Paint::red("No contract found. Please specify a contract and try again."));
                    std::process::exit(1);
                }
            }
            Err(e) => {
                tracing::error!(target: "cli", "PARSER ERRORED!");
                eprintln!("{}", Paint::red(&e));
                std::process::exit(1);
            }
        }
        return;
    }

    if let Some(TestCommands::Test { format, match_ }) = cli.test {
        match compiler.grab_contracts() {
            Ok(contracts) => {
                let match_ = Rc::new(match_);

                for contract in &contracts {
                    let tester = HuffTester::new(contract, Rc::clone(&match_));

                    let start = Instant::now();
                    match tester.execute() {
                        Ok(res) => {
                            print_test_report(res, ReportKind::from(&format), start);
                        }
                        Err(e) => {
                            eprintln!("{}", Paint::red(&e));
                            std::process::exit(1);
                        }
                    };
                }
            }
            Err(e) => {
                tracing::error!(target: "cli", "PARSER ERRORED!");
                eprintln!("{}", Paint::red(&e));
                std::process::exit(1);
            }
        }
        return;
    }

    let compile_res = compiler.execute();

    match compile_res {
        Ok(mut artifacts) => {
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
                                    dependencies: None,
                                })),
                            })
                            .collect::<Vec<Span>>(),
                    ),
                    token: None,
                });
                tracing::error!(target: "cli", "COMPILER ERRORED: {}", e);
                eprintln!("{}", Paint::red(&format!("{e}")));
                std::process::exit(1);
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
        Err(e) => {
            tracing::error!(target: "cli", "COMPILER ERRORED: {}", e);
            eprintln!("{}", Paint::red(&format!("{e}")));
            std::process::exit(1);
        }
    }
}

impl Huff {
    /// Preprocesses input files for compiling
    pub fn get_inputs(&self) -> Result<Vec<String>, CompilerError> {
        match &self.path {
            Some(path) => {
                tracing::debug!(target: "io", "FETCHING INPUT: {}", path);
                // If the file is huff, we can use it
                let ext = Path::new(&path).extension().unwrap_or_default();
                if ext.eq("huff") {
                    Ok(vec![path.clone()])
                } else {
                    // Otherwise, override the source files and use all files in the provided dir
                    unpack_files(path).map_err(CompilerError::FileUnpackError)
                }
            }
            None => {
                tracing::debug!(target: "io", "FETCHING SOURCE FILES: {}", self.source);
                // If there's no path, unpack source files
                unpack_files(&self.source).map_err(CompilerError::FileUnpackError)
            }
        }
    }
}
