//! Utils
//!
//! Refactored utilities commonly used across the huff-neo-rs project.

#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]

/// Abi Module
pub mod abi;

/// Artifact Module
pub mod artifact;

/// AST Module
pub mod ast;

/// Bytecode Traits Module
pub mod bytecode;

/// Token Module
pub mod token;

/// Lexing Error Module
pub mod error;

/// EVM Module
pub mod opcodes;

/// Lexical Reporting Module
pub mod report;

/// EVM Types Module
pub mod types;

/// Bytes Util Module
pub mod bytes_util;

/// Solidity Interface Generator
pub mod sol_interface;

/// Time Module
pub mod time;

/// Wasm Module
pub mod wasm;

/// EVM Version Module
pub mod evm_version;

/// File operations
pub mod file;

/// Prelude wraps common utilities.
pub mod prelude {
    pub use crate::file::span::*;
    pub use crate::file::unpack_files::*;
    pub use crate::{
        abi::*, artifact::*, ast::*, bytecode::*, bytes_util::*, error::*, evm_version::*, opcodes::*, report::*, sol_interface::*,
        token::*, types::*,
    };
}
