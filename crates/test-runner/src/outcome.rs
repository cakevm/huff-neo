use crate::errors::RunnerError;
use crate::runner::db_error;
use crate::spec::{AccountValue, ExpectedSlot, ExpectedValue};
use alloy_primitives::{U256, hex};
use huff_neo_utils::prelude::Contract;
use revm::DatabaseRef;
use serde::Serialize;

/// How the test call ended
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Outcome {
    /// The call stopped or returned, with the return data as hex
    Return { data: String },
    /// The call reverted, with the revert data as hex
    Revert { data: String },
    /// The call halted exceptionally, e.g. out of gas or on an invalid jump
    Halt { reason: String },
}

/// An expectation the test did not meet
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Failure {
    /// The decorator flag that set the expectation, or `outcome` for the default of returning
    pub expectation: String,
    /// What was expected and what happened instead
    pub message: String,
}

/// How a test is expected to end
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedOutcome {
    /// Returns, with exactly this data when given
    Return(Option<Vec<u8>>),
    /// Reverts, with exactly this data when given
    Revert(Option<Vec<u8>>),
    /// Reverts with data that starts with this
    PartialRevert(Vec<u8>),
    /// Halts exceptionally, for this reason when given
    Halt(Option<String>),
}

/// What a test's decorators expect of the test call
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expectations {
    /// How the test must end; a test without an outcome decorator must return
    pub outcome: ExpectedOutcome,
    /// The decorator flag that set the outcome, used to label a failure
    pub outcome_flag: Option<String>,
    /// Lower bound of the reported gas
    pub min_gas: Option<u64>,
    /// Upper bound of the reported gas
    pub max_gas: Option<u64>,
    /// Storage slots and the values they must hold after the test
    pub storage: Vec<ExpectedSlot>,
    /// Balances accounts must hold after the test
    pub balances: Vec<AccountValue<ExpectedValue>>,
}

impl Default for Expectations {
    fn default() -> Self {
        Self {
            outcome: ExpectedOutcome::Return(None),
            outcome_flag: None,
            min_gas: None,
            max_gas: None,
            storage: Vec::new(),
            balances: Vec::new(),
        }
    }
}

impl Expectations {
    /// The expectations the test call did not meet
    pub fn check(&self, outcome: &Outcome, gas: u64, contract: &Contract) -> Vec<Failure> {
        let mut failures = Vec::new();

        if let Some(message) = self.outcome_mismatch(outcome, contract) {
            let expectation = self.outcome_flag.as_deref().unwrap_or("outcome");
            failures.push(Failure { expectation: expectation.to_string(), message });
        }
        if let Some(min) = self.min_gas
            && gas < min
        {
            failures.push(Failure { expectation: String::from("expect_gas"), message: format!("used {gas} gas, less than min {min}") });
        }
        if let Some(max) = self.max_gas
            && gas > max
        {
            failures.push(Failure { expectation: String::from("expect_gas"), message: format!("used {gas} gas, more than max {max}") });
        }

        failures
    }

    /// The storage and balance expectations the state after the test does not meet
    pub fn check_post_state<DB>(&self, db: &DB) -> Result<Vec<Failure>, RunnerError>
    where
        DB: DatabaseRef,
        <DB as DatabaseRef>::Error: std::fmt::Debug,
    {
        let mut failures = Vec::new();

        for ExpectedSlot { address, slot, expected } in &self.storage {
            let actual = db.storage_ref(*address, *slot).map_err(db_error)?;
            if !expected.matches(actual) {
                failures.push(Failure {
                    expectation: String::from("expect_storage"),
                    message: format!("slot {slot:#x} at {address}: expected {}, got {actual:#x}", expected.describe(|v| format!("{v:#x}"))),
                });
            }
        }
        for AccountValue { address, value: expected } in &self.balances {
            let actual = db.basic_ref(*address).map_err(db_error)?.map_or(U256::ZERO, |account| account.balance);
            if !expected.matches(actual) {
                failures.push(Failure {
                    expectation: String::from("expect_balance"),
                    message: format!("balance of {address}: expected {}, got {actual}", expected.describe(|v| v.to_string())),
                });
            }
        }

        Ok(failures)
    }

    /// Describes how the outcome differs from the expected one, if it does
    fn outcome_mismatch(&self, outcome: &Outcome, contract: &Contract) -> Option<String> {
        match (&self.outcome, outcome) {
            // The test ended the expected way; compare the data or reason when one is expected
            (ExpectedOutcome::Return(expected), Outcome::Return { data }) => {
                let data = decode_hex(data);
                match expected {
                    Some(expected) if *expected != data => {
                        Some(format!("expected return data {}, got {}", display_hex(expected), display_hex(&data)))
                    }
                    _ => None,
                }
            }
            (ExpectedOutcome::Revert(expected), Outcome::Revert { data }) => {
                let data = decode_hex(data);
                match expected {
                    Some(expected) if *expected != data => Some(format!(
                        "expected revert data {}, got {}",
                        describe_revert(expected, contract),
                        describe_revert(&data, contract)
                    )),
                    _ => None,
                }
            }
            (ExpectedOutcome::PartialRevert(prefix), Outcome::Revert { data }) => {
                let data = decode_hex(data);
                (!data.starts_with(prefix)).then(|| {
                    format!(
                        "expected revert data starting with {}, got {}",
                        describe_revert(prefix, contract),
                        describe_revert(&data, contract)
                    )
                })
            }
            (ExpectedOutcome::Halt(expected), Outcome::Halt { reason }) => match expected {
                Some(expected) if !halt_reason_matches(expected, reason) => {
                    Some(format!("expected the test to halt with {expected}, but it halted with {reason}"))
                }
                _ => None,
            },

            // The test ended another way
            (ExpectedOutcome::Return(_), _) => Some(format!("expected the test to return, but it {}", describe_outcome(outcome, contract))),
            (ExpectedOutcome::Revert(_) | ExpectedOutcome::PartialRevert(_), _) => {
                Some(format!("expected a revert, but the test {}", describe_outcome(outcome, contract)))
            }
            (ExpectedOutcome::Halt(_), _) => Some(format!("expected the test to halt, but it {}", describe_outcome(outcome, contract))),
        }
    }
}

/// Whether a halt reason given in a decorator names the actual reason, so `OutOfGas` matches
/// every kind of out-of-gas halt
fn halt_reason_matches(expected: &str, actual: &str) -> bool {
    actual == expected || actual.strip_prefix(expected).is_some_and(|rest| rest.starts_with('('))
}

/// The outcome as the end of a sentence, e.g. "reverted with 0x82b42900 (Unauthorized)"
fn describe_outcome(outcome: &Outcome, contract: &Contract) -> String {
    match outcome {
        Outcome::Return { data } => format!("returned {}", display_hex(&decode_hex(data))),
        Outcome::Revert { data } => format!("reverted with {}", describe_revert(&decode_hex(data), contract)),
        Outcome::Halt { reason } => format!("halted with {reason}"),
    }
}

/// Revert data as hex, named after the contract's error when its selector matches one
fn describe_revert(data: &[u8], contract: &Contract) -> String {
    let error = data.get(..4).and_then(|selector| contract.errors.iter().find(|e| e.selector == selector));
    match error {
        Some(error) => format!("{} ({})", display_hex(data), error.name),
        None => display_hex(data),
    }
}

/// The bytes of an outcome's data, which the runner records as hex so it reads well in JSON
fn decode_hex(data: &str) -> Vec<u8> {
    hex::decode(data).unwrap_or_default()
}

/// Bytes as `0x`-prefixed hex, spelling out empty data so it is not mistaken for a missing value
fn display_hex(data: &[u8]) -> String {
    if data.is_empty() { String::from("0x (empty)") } else { format!("0x{}", hex::encode(data)) }
}
