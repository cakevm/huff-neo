use crate::ast::span::AstSpan;
use crate::prelude::Literal;
use serde::{Deserialize, Serialize};

/// An argument's location
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArgumentLocation {
    /// Memory location
    #[default]
    Memory,
    /// Storage location
    Storage,
    /// Calldata location
    Calldata,
}

/// A function or event argument
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Argument {
    /// Type of the argument
    pub arg_type: Option<String>,
    /// Optional Argument Location
    pub arg_location: Option<ArgumentLocation>,
    /// The name of the argument
    pub name: Option<String>,
    /// Is the argument indexed? TODO: should be valid for event arguments ONLY
    pub indexed: bool,
    /// The argument span
    pub span: AstSpan,
}

/// A Function Signature
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDefinition {
    /// The name of the function
    pub name: String,
    /// The function signature
    pub signature: [u8; 4],
    /// The parameters of the function
    pub inputs: Vec<Argument>,
    /// The function type
    pub fn_type: FunctionType,
    /// The return values of the function
    pub outputs: Vec<Argument>,
    /// The span of the function
    pub span: AstSpan,
}

/// Function Types
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionType {
    /// Viewable Function
    View,
    /// Payable Function
    Payable,
    /// Non Payable Function
    NonPayable,
    /// Pure Function
    Pure,
}

impl FunctionType {
    /// Get the string representation of the function type for usage in Solidity interface
    /// generation.
    pub fn interface_mutability(&self) -> &str {
        match self {
            FunctionType::View => " view",
            FunctionType::Pure => " pure",
            _ => "", // payable / nonpayable types not valid in Solidity interfaces
        }
    }
}

/// An Event Signature
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EventDefinition {
    /// The name of the event
    pub name: String,
    /// The parameters of the event
    pub parameters: Vec<Argument>,
    /// The event span
    pub span: AstSpan,
    /// The event hash
    pub hash: Literal,
}
