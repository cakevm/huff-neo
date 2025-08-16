use anvil::eth::backend::mem::inspector::AnvilInspector;
use comfy_table::{Cell, Color};
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
    #[serde(skip)]
    pub inspector: AnvilInspector,
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

/// A test report kind
pub enum ReportKind {
    /// Signals `gen_report` to format the test report as a table
    Table,
    /// Signals `gen_report` to format the test report as a list
    List,
    /// Signals `gen_report` to format the test report as JSON
    JSON,
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

/// Convert a shared reference to an `Option<String>` to a `ReportKind`.
/// If the `Option<String>` is `None` or there is no ReportKind match,
/// `ReportKind::List` is returned.
impl From<&Option<String>> for ReportKind {
    fn from(str: &Option<String>) -> Self {
        if let Some(str) = str {
            match str.to_lowercase().as_str() {
                "table" => ReportKind::Table,
                "list" => ReportKind::List,
                "json" => ReportKind::JSON,
                _ => ReportKind::List,
            }
        } else {
            ReportKind::List
        }
    }
}
