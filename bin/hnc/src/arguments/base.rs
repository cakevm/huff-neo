use crate::arguments::test::TestArgs;
use clap::{Parser, Subcommand, ValueEnum};
use huff_neo_utils::error::CompilerError;
use huff_neo_utils::file::unpack_files::unpack_files;
use std::io::Write;
use std::path::Path;
use yansi::Paint;

/// The Huff CLI Args
#[derive(Parser, Debug, Clone)]
#[clap(name = "hnc", about, long_about = None)]
pub struct HuffArgs {
    /// The contract(s) to compile.
    pub path: Option<String>,

    /// A directory whose Huff contracts are compiled when no path is given.
    #[clap(short = 's', long = "source-path")]
    pub source: Option<String>,

    /// The file the artifact is written to, with `-a`.
    #[clap(short = 'o', long = "output")]
    pub output: Option<String>,

    /// The directory the artifacts are written to, with `-a`.
    #[clap(short = 'd', long = "output-directory", default_value = "./artifacts")]
    pub outputdir: String,

    /// The constructor arguments, separated by spaces
    #[clap(short = 'i', long = "inputs", num_args = 1.., allow_negative_numbers = true)]
    pub inputs: Option<Vec<String>>,

    /// Prompt for the constructor arguments, then write the artifacts
    #[clap(short = 'n', long = "interactive")]
    pub interactive: bool,

    /// Whether to generate artifacts or not
    #[clap(short = 'a', long = "artifacts")]
    pub artifacts: bool,

    /// Apply branch relaxation to minimize deployment gas (use PUSH1 where possible)
    ///
    /// When enabled all pushes for jumps will be minimized to PUSH1 where possible.
    /// This can reduce deployment gas costs, but has no effect on runtime gas costs.
    /// Only applies to label references used in JUMPI and JUMP opcodes.
    #[clap(long = "relax-jumps")]
    pub relax_jumps: bool,

    /// Generate a Solidity interface next to each contract, optionally with this name
    #[clap(short = 'g', num_args = 0.., long = "interface")]
    pub interface: Option<String>,

    /// Generate and log bytecode.
    #[clap(short = 'b', long = "bytecode")]
    pub bytecode: bool,

    /// Generate and log runtime bytecode. Errors if CONSTRUCTOR returns custom bytes (runtime ≠ MAIN).
    #[clap(short = 'r', long = "bin-runtime")]
    pub bin_runtime: bool,

    /// Verbose output.
    #[clap(short = 'v', long = "verbose")]
    pub verbose: bool,

    /// Prints out the jump label PC indices for the specified contract.
    #[clap(short = 'l', long = "label-indices")]
    pub label_indices: bool,

    /// Output format for the label indices.
    #[clap(long = "format", value_enum, default_value_t = LabelFormat::Table, requires = "label_indices")]
    pub format: LabelFormat,

    /// Override / set constants for the compilation environment. Accepts hex values (0x...) or true/false.
    #[clap(short = 'c', long = "constants", num_args = 1..)]
    pub constants: Option<Vec<String>>,

    /// Compile a specific macro
    #[clap(short = 'm', long = "alt-main")]
    pub alternative_main: Option<String>,

    /// Compile a specific constructor macro
    #[clap(short = 't', long = "alt-constructor")]
    pub alternative_constructor: Option<String>,

    /// The EVM version to compile for, and to run tests with [default: osaka]
    #[clap(short = 'e', long = "evm-version", value_parser = ["paris", "shanghai", "cancun", "prague", "osaka", "amsterdam"])]
    pub evm_version: Option<String>,

    /// Skip the contract and initcode size limit checks (EIP-170/EIP-3860, raised by EIP-7954 in Amsterdam)
    #[clap(long = "no-size-limit")]
    pub no_size_limit: bool,

    /// Test subcommand
    #[clap(subcommand)]
    pub test: Option<TestCommands>,

    /// Output the flattened source code with all dependencies resolved
    #[clap(long = "flattened-source")]
    pub flattened_source: bool,

    /// Print version
    #[arg(short = 'V', long = "version")]
    pub version_long: bool,
}

/// Output format for the jump label PC indices
#[derive(ValueEnum, Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum LabelFormat {
    /// Human-readable table
    #[default]
    Table,
    /// Machine-readable JSON array
    Json,
}

#[derive(Subcommand, Clone, Debug)]
pub enum TestCommands {
    /// Test subcommand
    Test(TestArgs),
}

impl HuffArgs {
    /// The Huff files to compile: the given file, every file in the given directory, or every file
    /// in the source path when no path is given
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
                let source = self.source.as_deref().unwrap_or_default();
                tracing::debug!(target: "io", "FETCHING SOURCE FILES: {}", source);
                unpack_files(source).map_err(CompilerError::FileUnpackError)
            }
        }
    }
}

/// Helper function to read a stdin input
pub fn get_input(prompt: &str) -> String {
    print!("{} {prompt} ", Paint::blue(&"[INTERACTIVE]".to_string()));
    let mut input = String::new();
    let _ = std::io::stdout().flush();
    let _ = std::io::stdin().read_line(&mut input);

    input.trim().to_string()
}
