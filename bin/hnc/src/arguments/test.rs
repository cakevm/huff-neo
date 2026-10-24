use alloy_primitives::Address;
use clap::ArgAction;
use clap::Parser;
use foundry_cli::opts::EvmArgs;
use foundry_config::figment::value::{Dict, Map};
use foundry_config::figment::{Figment, Metadata, Profile, Provider};
use foundry_config::{Config, figment};
use huff_neo_test_runner::prelude::ReportKind;
use huff_neo_utils::shell::Verbosity;

/// The Test CLI Args
#[derive(Parser, Debug, Clone)]
pub struct TestArgs {
    /// The format of the test report
    #[clap(long = "format", value_enum, default_value_t = ReportKind::List)]
    pub format: ReportKind,

    /// Match a specific test by name.
    #[clap(short = 'm', long = "match")]
    pub match_: Option<String>,

    /// Run each test at this address instead of deploying it, e.g. to replay a deployed contract on a fork
    #[arg(long)]
    pub target_address: Option<Address>,

    /// Verbosity level of the test report, the same levels as forge.
    ///
    /// Pass multiple times to increase the verbosity (e.g. -v, -vv, -vvv).
    ///
    /// Verbosity levels:
    /// - 2 (-vv): Print console logs for all tests.
    /// - 3 (-vvv): Print execution traces for failing tests.
    /// - 4 (-vvvv): Print execution traces for all tests.
    /// - 5 (-vvvvv): Print execution traces for all tests, including storage changes.
    #[arg(help_heading = "Display options", global = true, short, long, verbatim_doc_comment, action = ArgAction::Count)]
    pub verbosity: Verbosity,

    /// Run a single test in the debugger. The matching test will be opened in the debugger regardless of the outcome of the test.
    #[arg(long)]
    pub debug: bool,

    /// Show the calls of Huff functions (`#define fn`) in traces, with their stack arguments and
    /// return values.
    #[arg(long)]
    pub decode_internal: bool,

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

/// The configuration of `hnc test`: the Foundry config of the project, overridden by the EVM flags
impl From<&TestArgs> for Figment {
    fn from(args: &TestArgs) -> Self {
        Config::figment().merge(&args.evm).merge(args)
    }
}
