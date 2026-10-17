use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::error::{CodegenError, CompilerError};
use revm::context::result::EVMError;
use std::convert::Infallible;
use thiserror::Error;

/// A Runner error
#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("Runner Error: {0}")]
    GenericError(String),

    #[error("Deployment Error: {0}")]
    DeploymentError(String),

    #[error("{0}")]
    InvalidDecorator(DecoratorError),

    #[error("Compiler Error: {0}")]
    CompilerError(CompilerError),

    #[error("Evm transact error with err={0}")]
    TransactError(String),

    #[error(transparent)]
    EVMError(#[from] EVMError<Infallible>),

    #[error(transparent)]
    EVMErrorDB(#[from] EVMError<foundry_evm::backend::DatabaseError>),
}

impl From<CodegenError> for RunnerError {
    fn from(error: CodegenError) -> Self {
        RunnerError::CompilerError(CompilerError::CodegenError(error))
    }
}

/// The ways a decorator flag can be invalid for the test runner
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecoratorErrorKind {
    /// The flag name is not a known decorator
    UnknownFlag,
    /// The flag has the wrong number of arguments, or an unknown or repeated `key =` argument
    InvalidArguments,
    /// An argument value has the wrong type or cannot be evaluated
    InvalidValue,
    /// A single-use flag is repeated, or a storage slot is set twice
    Duplicate,
    /// The flag contradicts another flag, such as two ways for the test to end
    Conflict,
}

/// A decorator flag the test runner cannot apply, pointing at the offending source
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("Invalid Decorator Flag: \"{flag}\"\n{}", span.error(Some(hint)))]
pub struct DecoratorError {
    /// The flag name
    pub flag: String,
    /// What is wrong with the flag
    pub kind: DecoratorErrorKind,
    /// How to fix the flag
    pub hint: String,
    /// The flag or argument, or both places for a duplicate
    pub span: AstSpan,
}
