use std::cmp::PartialOrd;
use std::fmt::Display;

/// EIP-170 runtime code size limit (24 KiB)
pub const EIP170_MAX_CODE_SIZE: usize = 0x6000;
/// EIP-3860 initcode size limit (48 KiB)
pub const EIP3860_MAX_INITCODE_SIZE: usize = 2 * EIP170_MAX_CODE_SIZE;
/// EIP-7954 runtime code size limit (64 KiB), active from Amsterdam
pub const EIP7954_MAX_CODE_SIZE: usize = 0x10000;
/// EIP-7954 initcode size limit (128 KiB), active from Amsterdam
pub const EIP7954_MAX_INITCODE_SIZE: usize = 2 * EIP7954_MAX_CODE_SIZE;

/// Evm Version
///
/// Determines which features will be available when compiling.

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
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
    Prague,
    /// Fulu/Osaka - Introduced CLZ
    ///
    ///
    /// Meta: <https://eips.ethereum.org/EIPS/eip-7607>
    /// CLZ: <https://eips.ethereum.org/EIPS/eip-7939>
    #[default]
    Osaka,
    /// Glamsterdam/Amsterdam - Introduced SLOTNUM, DUPN, SWAPN, EXCHANGE and raised the
    /// contract size limits
    ///
    /// Meta: <https://eips.ethereum.org/EIPS/eip-7773>
    /// SLOTNUM: <https://eips.ethereum.org/EIPS/eip-7843>
    /// DUPN/SWAPN/EXCHANGE: <https://eips.ethereum.org/EIPS/eip-8024>
    /// Contract size limits: <https://eips.ethereum.org/EIPS/eip-7954>
    Amsterdam,
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
            SupportedEVMVersions::Amsterdam => "amsterdam",
        };
        write!(f, "{}", version_str)
    }
}

#[derive(Debug, Default, Clone, Copy)]
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

    /// Check if the EVM version supports the SLOTNUM opcode - Amsterdam or later
    pub fn has_slotnum(&self) -> bool {
        self.version >= SupportedEVMVersions::Amsterdam
    }

    /// Check if the EVM version supports DUPN, SWAPN and EXCHANGE - Amsterdam or later
    pub fn has_stack_immediates(&self) -> bool {
        self.version >= SupportedEVMVersions::Amsterdam
    }

    /// Maximum size in bytes of deployed (runtime) contract code
    ///
    /// EIP-170 (24 KiB) before Amsterdam, EIP-7954 (64 KiB) from Amsterdam on.
    pub fn max_code_size(&self) -> usize {
        if self.version >= SupportedEVMVersions::Amsterdam { EIP7954_MAX_CODE_SIZE } else { EIP170_MAX_CODE_SIZE }
    }

    /// Maximum size in bytes of contract creation (init) code
    ///
    /// EIP-3860 (48 KiB) before Amsterdam, EIP-7954 (128 KiB) from Amsterdam on.
    pub fn max_initcode_size(&self) -> usize {
        if self.version >= SupportedEVMVersions::Amsterdam { EIP7954_MAX_INITCODE_SIZE } else { EIP3860_MAX_INITCODE_SIZE }
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
            "amsterdam" => Self::new(SupportedEVMVersions::Amsterdam),
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
