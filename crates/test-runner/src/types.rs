use crate::internal::HuffFunction;
use crate::outcome::{Failure, Outcome};
use alloy_primitives::Log;
use clap::ValueEnum;
use comfy_table::{Cell, Color};
use foundry_evm::traces::SparsedTraceArena;
use huff_neo_utils::artifact::SourceMapEntry;
use serde::Serialize;
use yansi::Paint;

/// A test result
#[derive(Debug, Clone, Serialize)]
pub struct TestResult {
    pub name: String,
    pub return_data: Option<String>,
    pub gas: u64,
    pub status: TestStatus,
    pub revert_reason: Option<String>,
    /// Whether the test met every expectation; the same as `status` being `Success`
    pub passed: bool,
    /// How the test call ended
    pub outcome: Outcome,
    /// The expectations the test did not meet
    pub failures: Vec<Failure>,
    /// The call traces of the test, as far as the runner records them
    #[serde(skip)]
    pub traces: Option<SparsedTraceArena>,
    /// The logs the test emitted, when the runner collects them
    #[serde(skip)]
    pub logs: Option<Vec<Log>>,
    /// The Huff functions of the test, which traces show as internal calls
    #[serde(skip)]
    pub functions: Vec<HuffFunction>,
    /// Source map for the test bytecode (for debugging)
    #[serde(skip)]
    pub source_map: Option<Vec<SourceMapEntry>>,
    /// Deployed contract address
    #[serde(skip)]
    pub address: Option<alloy_primitives::Address>,
    /// Source code for debugging
    #[serde(skip)]
    pub source_code: Option<String>,
    /// Bytecode for debugging
    #[serde(skip)]
    pub bytecode: Option<String>,
    /// Multiple source files for multi-file debugging
    /// Vec of (file_path, source_content)
    #[serde(skip)]
    pub source_files: Vec<(String, String)>,
}

/// A test status variant
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TestStatus {
    Success,
    Revert,
}

/// The format of the test report
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, ValueEnum)]
pub enum ReportKind {
    /// Each test with its failures, logs and traces
    #[default]
    List,
    /// A table of the tests and their gas
    Table,
    /// Machine-readable JSON
    Json,
}

impl TestStatus {
    /// The status of a test that met, or did not meet, every expectation
    pub fn from_passed(passed: bool) -> Self {
        if passed { Self::Success } else { Self::Revert }
    }
}

/// Convert a TestStatus variant directly to a colored string for use in the report
impl From<TestStatus> for String {
    fn from(status: TestStatus) -> Self {
        match status {
            TestStatus::Success => Paint::green("PASS").to_string(),
            TestStatus::Revert => Paint::red("FAIL").to_string(),
        }
    }
}

/// Convert a TestStatus variant directly to a table cell for use in the report
impl From<TestStatus> for Cell {
    fn from(status: TestStatus) -> Self {
        match status {
            TestStatus::Success => Cell::new("PASS").fg(Color::Green),
            TestStatus::Revert => Cell::new("FAIL").fg(Color::Red),
        }
    }
}
