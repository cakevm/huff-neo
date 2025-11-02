/// Defines a context in which the lexing happens.
/// Allows to differentiate between EVM types and opcodes that can either
/// be identical or the latter being a substring of the former (example : bytes32 and byte)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Context {
    /// global context
    Global,
    /// Macro definition context
    MacroDefinition,
    /// Macro's body context
    MacroBody,
    /// Macro's argument context (definition or being called)
    MacroArgs,
    /// ABI context
    Abi,
    /// Lexing args of functions inputs/outputs and events
    AbiArgs,
    /// constant context
    Constant,
    /// Code table context
    CodeTableBody,
    /// Built-in function context
    BuiltinFunction,
    /// For loop body context
    ForLoopBody,
}
