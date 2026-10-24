//! ## Abi
//!
//! A module containing ABI type definitions for ethereum contracts.
//!
//! #### Usage
//!
//! Let's say you have a `Contract<'a>` generated from the [huff_parser](./huff_parser),
//! representing an AST. This crate lets you easily convert the `Contract` into an `Abi`
//! instance like so:
//!
//! ```rust
//! use std::sync::{Arc, Mutex};
//! use indexmap::IndexMap;
//! use huff_neo_utils::ast::abi::FunctionType;
//! use huff_neo_utils::ast::span::AstSpan;
//! use huff_neo_utils::prelude::*;
//!
//! // Generate a default contract for demonstrative purposes.
//! // Realistically, contract generation would be done as shown in [huff_parser](./huff_parser)
//! let contract = Contract {
//!     macros: IndexMap::new(),
//!     invocations: vec![],
//!     imports: vec![],
//!     constants: Arc::new(Mutex::new(vec![])),
//!     errors: vec![],
//!     functions: vec![huff_neo_utils::ast::abi::FunctionDefinition {
//!         name: "CONSTRUCTOR".to_string(),
//!         signature: [0u8, 0u8, 0u8, 0u8],
//!         inputs: vec![],
//!         fn_type: FunctionType::NonPayable,
//!         outputs: vec![],
//!         span: AstSpan(vec![]),
//!     }],
//!     events: vec![],
//!     tables: vec![],
//!     labels: Default::default(),
//!     flattened_source: None,
//!     source_files: vec![],
//!     source_map: vec![],
//!     runtime_size: None,
//! };
//!
//! // Create an ABI using that generate contract
//! let abi: Abi = contract.into();
//! ```

use crate::ast;
use crate::ast::abi::FunctionType;
use crate::ast::huff::{self};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt};

/// #### Abi
///
/// The ABI of the generated code.
#[derive(Default, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct Abi {
    /// The constructor
    pub constructor: Option<Constructor>,
    /// A list of functions and their definitions
    pub functions: BTreeMap<String, Function>,
    /// A list of events and their definitions
    pub events: BTreeMap<String, Event>,
    /// A list of errors and their definitions
    pub errors: BTreeMap<String, Error>,
    /// If the contract defines receive logic
    pub receive: bool,
    /// If the contract defines fallback logic
    pub fallback: bool,
}

impl Abi {
    /// Public associated function to instantiate a new Abi.
    pub fn new() -> Self {
        Self::default()
    }
}

// Allows for simple ABI Generation by directly translating the AST
impl From<huff::Contract> for Abi {
    fn from(contract: huff::Contract) -> Self {
        // The inputs of `#define function constructor(...)`. A CONSTRUCTOR macro without that
        // declaration takes no arguments; its macro parameters are not ABI inputs.
        let constructor = contract
            .functions
            .iter()
            .find(|function| function.name.eq_ignore_ascii_case("constructor"))
            .map(|func| Constructor {
                inputs: func
                    .inputs
                    .iter()
                    .map(|argument| FunctionParam {
                        name: argument.name.clone().unwrap_or_default(),
                        kind: argument.arg_type.clone().unwrap_or_default().into(),
                        internal_type: None,
                    })
                    .collect(),
            })
            .or_else(|| contract.macros.contains_key("CONSTRUCTOR").then(|| Constructor { inputs: vec![] }));

        // Instantiate functions and events
        let mut functions = BTreeMap::new();
        let mut events = BTreeMap::new();
        let mut errors = BTreeMap::new();

        // Translate contract functions
        // Excluding constructor
        functions.extend(
            contract
                .functions
                .iter()
                .filter(|function: &&ast::abi::FunctionDefinition| !function.name.eq_ignore_ascii_case("constructor"))
                .map(|function| {
                    (
                        function.name.to_string(),
                        Function {
                            name: function.name.to_string(),
                            inputs: function
                                .inputs
                                .iter()
                                .map(|argument| FunctionParam {
                                    name: argument.name.clone().unwrap_or_default(),
                                    kind: argument.arg_type.clone().unwrap_or_default().into(),
                                    internal_type: None,
                                })
                                .collect(),
                            outputs: function
                                .outputs
                                .iter()
                                .map(|argument| FunctionParam {
                                    name: argument.name.clone().unwrap_or_default(),
                                    kind: argument.arg_type.clone().unwrap_or_default().into(),
                                    internal_type: None,
                                })
                                .collect(),
                            constant: false,
                            state_mutability: function.fn_type.clone(),
                        },
                    )
                }),
        );

        // Translate contract events
        events.extend(contract.events.iter().map(|event| {
            (
                event.name.to_string(),
                Event {
                    name: event.name.to_string(),
                    inputs: event
                        .parameters
                        .iter()
                        .map(|argument| EventParam {
                            name: argument.name.clone().unwrap_or_default(),
                            kind: argument.arg_type.clone().unwrap_or_default().into(),
                            indexed: argument.indexed,
                        })
                        .collect(),
                    anonymous: false,
                },
            )
        }));

        // Translate contract errors
        errors.extend(contract.errors.iter().map(|error| {
            (
                error.name.to_string(),
                Error {
                    name: error.name.to_string(),
                    inputs: error
                        .parameters
                        .iter()
                        .map(|argument| FunctionParam {
                            name: argument.name.clone().unwrap_or_default(),
                            kind: argument.arg_type.clone().unwrap_or_default().into(),
                            internal_type: None,
                        })
                        .collect(),
                },
            )
        }));

        Self { constructor, functions, events, errors, receive: false, fallback: false }
    }
}

/// #### Function
///
/// A function definition.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct Function {
    /// The function name
    pub name: String,
    /// The function inputs
    pub inputs: Vec<FunctionParam>,
    /// The function outputs
    pub outputs: Vec<FunctionParam>,
    /// Constant
    pub constant: bool,
    /// The state mutability
    pub state_mutability: FunctionType,
}

/// #### Event
///
/// An Event definition.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct Event {
    /// The event name
    pub name: String,
    /// The event inputs
    pub inputs: Vec<EventParam>,
    /// Anonymity
    pub anonymous: bool,
}

/// #### EventParam
///
/// Event parameters.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct EventParam {
    /// The parameter name
    pub name: String,
    /// The parameter type
    pub kind: FunctionParamType,
    /// If the parameter is indexed
    pub indexed: bool,
}

/// #### Error
///
/// An Error definition.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct Error {
    /// The error name
    pub name: String,
    /// The error inputs
    pub inputs: Vec<FunctionParam>,
}

/// #### Constructor
///
/// The contract constructor
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct Constructor {
    /// Constructor inputs
    pub inputs: Vec<FunctionParam>,
}

/// #### FunctionParam
///
/// A generic function parameter
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub struct FunctionParam {
    /// The function parameter name
    pub name: String,
    /// The function parameter type
    pub kind: FunctionParamType,
    /// The internal type of the parameter
    pub internal_type: Option<String>,
}

/// #### FunctionParamType
///
/// The type of function parameter
#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "wasm", derive(tsify::Tsify))]
#[cfg_attr(feature = "wasm", tsify(into_wasm_abi, from_wasm_abi))]
pub enum FunctionParamType {
    /// An address
    Address,
    /// Bytes
    Bytes,
    /// A signed integer
    Int(usize),
    /// An unsigned integer
    Uint(usize),
    /// A boolean
    Bool,
    /// A String
    String,
    /// Array ; `uint256[2][]` => `Array(Uint(256), [2, 0])`
    Array(Box<FunctionParamType>, Vec<usize>),
    /// Fixed number of bytes
    FixedBytes(usize),
    /// A tuple of parameters
    Tuple(Vec<FunctionParamType>),
}

impl FunctionParamType {
    /// Checks if the param type should be designated as "memory" for solidity interface
    /// generation.
    pub fn is_memory_type(&self) -> bool {
        matches!(self, FunctionParamType::Bytes | FunctionParamType::String | FunctionParamType::Tuple(_) | FunctionParamType::Array(_, _))
    }
}

impl fmt::Debug for FunctionParamType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f)
    }
}

impl fmt::Display for FunctionParamType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f)
    }
}

impl FunctionParamType {
    /// Print a function parameter type to a formatter
    pub fn display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            FunctionParamType::Address => write!(f, "address"),
            FunctionParamType::Bytes => write!(f, "bytes"),
            FunctionParamType::Int(size) => write!(f, "int{size}"),
            FunctionParamType::Uint(size) => write!(f, "uint{size}"),
            FunctionParamType::Bool => write!(f, "bool"),
            FunctionParamType::String => write!(f, "string"),
            FunctionParamType::Array(fpt, sizes) => write!(
                f,
                "{}{}",
                fpt,
                sizes.iter().map(|s| { if !s.eq(&0) { format!("[{s}]") } else { "[]".to_string() } }).collect::<Vec<_>>().join("")
            ),
            FunctionParamType::FixedBytes(size) => write!(f, "bytes{size}"),
            FunctionParamType::Tuple(inner) => write!(f, "({})", inner.iter().map(|fpt| fpt.to_string()).collect::<Vec<_>>().join(",")),
        }
    }

    /// Convert string to type
    pub fn convert_string_to_type(string: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let input = string.trim().to_lowercase();
        // Array sizes are the trailing `[n]` or `[]` groups, so brackets inside a tuple stay with it
        if input.ends_with(']') {
            let mut base = input.as_str();
            let mut sizes = Vec::new();
            while let Some(rest) = base.strip_suffix(']') {
                let open = rest.rfind('[').ok_or_else(|| format!("Failed to create FunctionParamType from string: {string}"))?;
                let size = &rest[open + 1..];
                sizes.push(if size.is_empty() { 0 } else { size.parse::<usize>()? });
                base = &rest[..open];
            }
            sizes.reverse();
            return Ok(Self::Array(Box::new(FunctionParamType::convert_string_to_type(base)?), sizes));
        }
        if let Some(inner) = input.strip_prefix('(').and_then(|rest| rest.strip_suffix(')')) {
            return Ok(Self::Tuple(split_tuple_components(inner).into_iter().map(Self::convert_string_to_type).collect::<Result<_, _>>()?));
        }
        if input.starts_with("uint") {
            // Default to 256 if no size
            let size = match input.get(4..input.len()) {
                Some(s) => match s.is_empty() {
                    false => s.parse::<usize>().unwrap(),
                    true => 256,
                },
                None => 256,
            };
            return Ok(Self::Uint(size));
        }
        if input.starts_with("int") {
            // Default to 256 if no size
            let size = match input.get(3..input.len()) {
                Some(s) => match s.is_empty() {
                    false => s.parse::<usize>().unwrap(),
                    true => 256,
                },
                None => 256,
            };
            return Ok(Self::Int(size));
        }
        if input.starts_with("bytes") && input.len() != 5 {
            let size = input.get(5..input.len()).unwrap().parse::<usize>().unwrap();
            return Ok(Self::FixedBytes(size));
        }
        if input.starts_with("bool") {
            return Ok(Self::Bool);
        }
        if input.starts_with("address") {
            return Ok(Self::Address);
        }
        if input.starts_with("string") {
            return Ok(Self::String);
        }
        if input == "bytes" {
            Ok(Self::Bytes)
        } else {
            tracing::error!("Failed to create FunctionParamType from string: {}", string);
            Err(format!("Failed to create FunctionParamType from string: {string}"))?
        }
    }
}

/// Splits the components of a tuple type at its top-level commas
fn split_tuple_components(inner: &str) -> Vec<&str> {
    let mut components = Vec::new();
    let (mut depth, mut start) = (0usize, 0);
    for (i, c) in inner.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                components.push(&inner[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    components.push(&inner[start..]);
    components
}

impl From<&str> for FunctionParamType {
    fn from(string: &str) -> Self {
        FunctionParamType::convert_string_to_type(string).unwrap()
    }
}

impl From<String> for FunctionParamType {
    fn from(string: String) -> Self {
        FunctionParamType::convert_string_to_type(&string).unwrap()
    }
}
