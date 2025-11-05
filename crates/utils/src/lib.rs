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

/// Push Value Module
pub mod push_value;

/// Solidity Interface Generator
pub mod sol_interface;

/// Time Module
pub mod time;

/// Wasm Module
pub mod wasm;

/// EVM Version Module
pub mod evm_version;

/// Builtin Function Evaluation Module
pub mod builtin_eval;

/// File operations
pub mod file;

/// Shell attributes
pub mod shell;

/// Lexer Context
pub mod lexer_context;

/// Prelude wraps common utilities.
pub mod prelude {
    pub use crate::ast::{abi::*, huff::*, span::*};
    pub use crate::file::span::*;
    pub use crate::file::unpack_files::*;
    pub use crate::file::{file_source::*, full_file_source::*};
    pub use crate::{
        abi::*, artifact::*, bytecode::*, bytes_util::*, error::*, evm_version::*, opcodes::*, push_value::*, report::*, sol_interface::*,
        token::*, types::*,
    };
}
