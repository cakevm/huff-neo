use huff_neo_utils::error::CompilerError;
use revm::primitives::EVMError;
use std::convert::Infallible;
use thiserror::Error;

/// A Runner error
#[derive(Debug, Error)]
pub enum RunnerError {
    #[error("Runner Error: {0}")]
    GenericError(String),

    #[error("Deployment Error: {0}")]
    DeploymentError(String),

    #[error("Compiler Error: {0}")]
    CompilerError(CompilerError),

    #[error("Evm transact error with err={0}")]
    TransactError(String),

    #[error(transparent)]
    EVMError(#[from] EVMError<Infallible>),

    #[error(transparent)]
    EVMErrorDB(#[from] EVMError<foundry_evm::backend::DatabaseError>),
}
