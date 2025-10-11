use alloy_primitives::Address;
use alloy_primitives::private::serde::Serialize;
use clap::Parser;
use foundry_cli::opts::EnvArgs;
use foundry_config::figment::error::Kind::InvalidType;
use foundry_config::figment::value::{Dict, Map, Value};
use foundry_config::figment::{Figment, Metadata, Profile, Provider};
use foundry_config::{Config, figment};

// Adapted/Copy from https://github.com/foundry-rs/foundry/blob/1d5fa64/crates/common/src/evm.rs#L46

#[derive(Clone, Debug, Default, Serialize, Parser)]
#[command(next_help_heading = "EVM options", about = None, long_about = None)] // override doc
pub struct EvmArgs {
    /// Fetch state over a remote endpoint instead of starting from an empty state.
    ///
    /// If you want to fetch state from a specific block number, see --fork-block-number.
    #[arg(long, short, visible_alias = "rpc-url", value_name = "URL")]
    #[serde(rename = "eth_rpc_url", skip_serializing_if = "Option::is_none")]
    pub fork_url: Option<String>,

    /// Fetch state from a specific block number over a remote endpoint.
    ///
    /// See --fork-url.
    #[arg(long, requires = "fork_url", value_name = "BLOCK")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fork_block_number: Option<u64>,

    /// Number of retries.
    ///
    /// See --fork-url.
    #[arg(long, requires = "fork_url", value_name = "RETRIES")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fork_retries: Option<u32>,

    /// Initial retry backoff on encountering errors.
    ///
    /// See --fork-url.
    #[arg(long, requires = "fork_url", value_name = "BACKOFF")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fork_retry_backoff: Option<u64>,

    /// Explicitly disables the use of RPC caching.
    ///
    /// All storage slots are read entirely from the endpoint.
    ///
    /// This flag overrides the project's configuration file.
    ///
    /// See --fork-url.
    #[arg(long)]
    #[serde(skip)]
    pub no_storage_caching: bool,

    /// The address which will be executing tests/scripts.
    #[arg(long, value_name = "ADDRESS")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sender: Option<Address>,

    /// Sets the number of assumed available compute units per second for this provider
    ///
    /// default value: 330
    ///
    /// See also --fork-url and <https://docs.alchemy.com/reference/compute-units#what-are-cups-compute-units-per-second>
    #[arg(long, alias = "cups", value_name = "CUPS", help_heading = "Fork config")]
    pub compute_units_per_second: Option<u64>,

    /// Disables rate limiting for this node's provider.
    ///
    /// See also --fork-url and <https://docs.alchemy.com/reference/compute-units#what-are-cups-compute-units-per-second>
    #[arg(long, value_name = "NO_RATE_LIMITS", help_heading = "Fork config", visible_alias = "no-rate-limit")]
    #[serde(skip)]
    pub no_rpc_rate_limit: bool,

    /// All ethereum environment related arguments
    #[command(flatten)]
    #[serde(flatten)]
    pub env: EnvArgs,
}

impl Provider for EvmArgs {
    fn metadata(&self) -> Metadata {
        Metadata::named("Evm Opts Provider")
    }

    fn data(&self) -> Result<Map<Profile, Dict>, figment::Error> {
        let value = Value::serialize(self)?;
        let error = InvalidType(value.to_actual(), "map".into());
        let dict = value.into_dict().ok_or(error)?;

        Ok(Map::from([(Config::selected_profile(), dict)]))
    }
}

impl From<&EvmArgs> for Figment {
    fn from(value: &EvmArgs) -> Self {
        Config::figment().merge(value)
    }
}
