use std::cmp::PartialOrd;

/// Evm Version
///
/// Determines which features will be available when compiling.

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub enum SupportedEVMVersions {
    /// Introduced prevrandao, disallow difficulty opcode (does not affect codegen)
    Paris,
    /// Introduced Push0
    Shanghai,
    /// Introduced TLOAD, TSTORE, MCOPY, BLOBHASH, and BLOBBASEFEE
    #[default]
    Cancun,
    /// Introduces EOA account code
    Prague,
    /// Introduces EOF
    Osaka,
}

#[derive(Debug, Default)]
/// EVM Version
pub struct EVMVersion {
    version: SupportedEVMVersions,
}

impl EVMVersion {
    /// Create a new EVM Version with the specified value
    pub fn new(version: SupportedEVMVersions) -> Self {
        Self { version }
    }

    /// As PartialOrd is implemented in the struct, all versions after shanghai will support this
    pub fn has_push0(&self) -> bool {
        self.version >= SupportedEVMVersions::Shanghai
    }
}

/// Convert from `Option<String>` to EVMVersion
impl From<Option<String>> for EVMVersion {
    fn from(version: Option<String>) -> Self {
        match version {
            Some(version) => Self::from(version),
            None => Self::default(),
        }
    }
}

/// Convert from String to EVMVersion
impl From<String> for EVMVersion {
    fn from(version: String) -> Self {
        match version.as_str() {
            "shanghai" => Self::new(SupportedEVMVersions::Shanghai),
            "paris" => Self::new(SupportedEVMVersions::Paris),
            "cancun" => Self::new(SupportedEVMVersions::Cancun),
            "prague" => Self::new(SupportedEVMVersions::Prague),
            "osaka" => Self::new(SupportedEVMVersions::Osaka),
            _ => Self::default(),
        }
    }
}
