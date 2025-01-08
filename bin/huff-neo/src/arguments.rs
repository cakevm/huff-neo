use clap::{Parser, Subcommand};
use huff_neo_utils::error::CompilerError;
use huff_neo_utils::io::unpack_files;
use std::io::Write;
use std::path::Path;
use yansi::Paint;

/// The Huff CLI Args
#[derive(Parser, Debug, Clone)]
#[clap(name = "huff-neo", version, about, long_about = None)]
pub struct HuffArgs {
    /// The contract(s) to compile.
    pub path: Option<String>,

    /// The contracts source path.
    #[clap(short = 's', long = "source-path", default_value = "./contracts")]
    pub source: String,

    /// The output file path.
    #[clap(short = 'o', long = "output")]
    pub output: Option<String>,

    /// The output directory.
    #[clap(short = 'd', long = "output-directory", default_value = "./artifacts")]
    pub outputdir: String,

    /// The input constructor arguments
    #[clap(short = 'i', long = "inputs", num_args = 1..)]
    pub inputs: Option<Vec<String>>,

    /// Interactively input the constructor args
    #[clap(short = 'n', long = "interactive")]
    pub interactive: bool,

    /// Whether to generate artifacts or not
    #[clap(short = 'a', long = "artifacts")]
    pub artifacts: bool,

    /// Optimize compilation [WIP]
    #[clap(short = 'z', long = "optimize")]
    pub optimize: bool,

    /// Generate solidity interface for a Huff artifact
    #[clap(short = 'g', num_args = 0.., long = "interface")]
    pub interface: Option<String>,

    /// Generate and log bytecode.
    #[clap(short = 'b', long = "bytecode")]
    pub bytecode: bool,

    /// Generate and log runtime bytecode.
    #[clap(short = 'r', long = "bin-runtime")]
    pub bin_runtime: bool,

    /// Prints out to the terminal.
    #[clap(short = 'p', long = "print")]
    pub print: bool,

    /// Verbose output.
    #[clap(short = 'v', long = "verbose")]
    pub verbose: bool,

    /// Prints out the jump label PC indices for the specified contract.
    #[clap(short = 'l', long = "label-indices")]
    pub label_indices: bool,

    /// Override / set constants for the compilation environment.
    #[clap(short = 'c', long = "constants", num_args = 1..)]
    pub constants: Option<Vec<String>>,

    /// Compile a specific macro
    #[clap(short = 'm', long = "alt-main")]
    pub alternative_main: Option<String>,

    /// Compile a specific constructor macro
    #[clap(short = 't', long = "alt-constructor")]
    pub alternative_constructor: Option<String>,

    /// Set the EVM version
    #[clap(short = 'e', long = "evm-version")]
    pub evm_version: Option<String>,

    /// Test subcommand
    #[clap(subcommand)]
    pub test: Option<TestCommands>,
}

#[derive(Subcommand, Clone, Debug)]
pub enum TestCommands {
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

impl HuffArgs {
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

/// Helper function to read a stdin input
pub fn get_input(prompt: &str) -> String {
    print!("{} {prompt} ", Paint::blue(&"[INTERACTIVE]".to_string()));
    let mut input = String::new();
    let _ = std::io::stdout().flush();
    let _ = std::io::stdin().read_line(&mut input);

    input.trim().to_string()
}
