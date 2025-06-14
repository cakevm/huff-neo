use crate::arguments::evm::EvmArgs;
use alloy_primitives::Address;
use clap::ArgAction;
use clap::Parser;
use foundry_config::figment::value::{Dict, Map};
use foundry_config::figment::{Metadata, Profile, Provider};
use foundry_config::{Config, figment};
use huff_neo_utils::shell::Verbosity;

foundry_config::merge_impl_figment_convert!(TestArgs, evm);

/// The Test CLI Args
#[derive(Parser, Debug, Clone)]
pub struct TestArgs {
    /// Format the test output as a list, table, or JSON.
    #[clap(long = "format")]
    pub format: Option<String>,

    /// Match a specific test by name.
    #[clap(short = 'm', long = "match")]
    pub match_: Option<String>,

    /// Target address for the contract
    #[arg(long)]
    pub target_address: Option<Address>,

    /// Verbosity level
    #[arg(help_heading = "Display options", global = true, short, long, verbatim_doc_comment, action = ArgAction::Count)]
    pub verbosity: Verbosity,

    /// Run a single test in the debugger. The matching test will be opened in the debugger regardless of the outcome of the test.
    #[arg(long)]
    pub debug: bool,

    /// Identify internal functions in traces.
    ///
    /// This will trace internal functions and decode stack parameters.
    ///
    /// Parameters stored in memory (such as bytes or arrays) are currently decoded only when a
    /// single function is matched, similarly to `--debug`, for performance reasons.
    #[arg(long)]
    pub decode_internal: bool,

    /// Print a gas report.
    #[arg(long, env = "FORGE_GAS_REPORT")]
    pub gas_report: bool,

    /// Anvil node parameters
    #[command(flatten)]
    pub evm: EvmArgs,
}

impl Provider for TestArgs {
    fn metadata(&self) -> Metadata {
        Metadata::named("TestArgs Provider")
    }

    fn data(&self) -> Result<Map<Profile, Dict>, figment::Error> {
        Ok(Map::from([(Config::selected_profile(), Dict::default())]))
    }
}
