use std::cmp::PartialOrd;
use std::fmt::Display;

/// Evm Version
///
/// Determines which features will be available when compiling.

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub enum SupportedEVMVersions {
    /// Introduced PREVRANDAO, disallow difficulty opcode (does not affect codegen)
    Paris,
    /// Introduced PUSH0
    Shanghai,
    /// Deneb/Cancun - Introduced TLOAD, TSTORE, MCOPY, BLOBHASH, and BLOBBASEFEE
    ///
    /// Meta: <https://eips.ethereum.org/EIPS/eip-7569>
    /// TLOAD/TSTORE: <https://eips.ethereum.org/EIPS/eip-1153>
    /// MCOPY: <https://eips.ethereum.org/EIPS/eip-5656>
    /// BLOBHASH: <https://eips.ethereum.org/EIPS/eip-4844>
    /// BLOBBASEFEE: <https://eips.ethereum.org/EIPS/eip-7516>
    Cancun,
    /// Prague/Electra - No new opcodes
    ///
    /// Meta: <https://eips.ethereum.org/EIPS/eip-7600>
    #[default]
    Prague,
    /// Fulu/Osaka - Introduced CLZ
    ///
    ///
    /// Meta: <https://eips.ethereum.org/EIPS/eip-7607>
    /// CLZ: <https://eips.ethereum.org/EIPS/eip-7939>
    Osaka,
}

/// Display SupportedEVMVersions as string
impl Display for SupportedEVMVersions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let version_str = match self {
            SupportedEVMVersions::Shanghai => "shanghai",
            SupportedEVMVersions::Paris => "paris",
            SupportedEVMVersions::Cancun => "cancun",
            SupportedEVMVersions::Prague => "prague",
            SupportedEVMVersions::Osaka => "osaka",
        };
        write!(f, "{}", version_str)
    }
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

    /// Get the current EVM version
    pub fn version(&self) -> &SupportedEVMVersions {
        &self.version
    }

    /// As PartialOrd is implemented in the struct, all versions after shanghai will support this
    pub fn has_push0(&self) -> bool {
        self.version >= SupportedEVMVersions::Shanghai
    }

    /// Check if the EVM version supports prevrandao - Paris or later
    pub fn has_prevrandao(&self) -> bool {
        self.version >= SupportedEVMVersions::Paris
    }

    /// Check if the EVM version supports transient storage (TLOAD, TSTORE) - Cancun or later
    pub fn has_transient_storage(&self) -> bool {
        self.version >= SupportedEVMVersions::Cancun
    }

    /// Check if the EVM version supports MCOPY opcode - Cancun or later
    pub fn has_mcopy(&self) -> bool {
        self.version >= SupportedEVMVersions::Cancun
    }

    /// Check if the EVM version supports blob opcodes (BLOBHASH, BLOBBASEFEE) - Cancun or later
    pub fn has_blob_opcodes(&self) -> bool {
        self.version >= SupportedEVMVersions::Cancun
    }

    /// Check if the EVM version supports CLZ opcode - Osaka or later
    pub fn has_clz(&self) -> bool {
        self.version >= SupportedEVMVersions::Osaka
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

/// Display EVMVersion as string
impl Display for EVMVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.version)
    }
}
