use crate::Codegen;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::prelude::{
    CodegenError, CodegenErrorKind, ConstVal, ConstantDefinition, Contract, EVMVersion, literal_gen, str_to_bytes32,
};

/// Transforms a constant definition into it's respective bytecode
pub fn constant_gen(evm_version: &EVMVersion, name: &str, contract: &Contract, ir_byte_span: &AstSpan) -> Result<String, CodegenError> {
    let constant = lookup_constant(name, contract, ir_byte_span)?;

    // Generate bytecode for the constant
    // Should always be a `Literal` if storage pointers were derived in the AST
    // prior to generating the IR bytes.
    tracing::info!(target: "codegen", "FOUND CONSTANT DEFINITION: {}", constant.name);
    let push_bytes = match &constant.value {
        ConstVal::Bytes(bytes) => {
            let hex_literal = str_to_bytes32(bytes.0.as_str());
            literal_gen(evm_version, &hex_literal)
        }
        ConstVal::StoragePointer(sp) => literal_gen(evm_version, sp),
        ConstVal::BuiltinFunctionCall(bf) => Codegen::gen_builtin_bytecode(evm_version, contract, bf, ir_byte_span.clone())?,
        ConstVal::Expression(expr) => {
            tracing::info!(target: "codegen", "EVALUATING CONSTANT EXPRESSION FOR \"{}\"", constant.name);
            let evaluated = contract.evaluate_constant_expression(expr)?;
            literal_gen(evm_version, &evaluated)
        }
        ConstVal::Noop => {
            tracing::info!(target: "codegen", "CONSTANT \"{}\" IS __NOOP - GENERATING NO BYTECODE", constant.name);
            String::new() // Generate no bytecode
        }
        ConstVal::FreeStoragePointer(fsp) => {
            // If this is reached in codegen stage, the `derive_storage_pointers`
            // method was not called on the AST.
            tracing::error!(target: "codegen", "STORAGE POINTERS INCORRECTLY DERIVED FOR \"{:?}\"", fsp);
            return Err(CodegenError { kind: CodegenErrorKind::StoragePointersNotDerived, span: constant.span.clone_box(), token: None });
        }
    };

    Ok(push_bytes)
}

/// Looks up a constant definition by name
pub fn lookup_constant(name: &str, contract: &Contract, ir_byte_span: &AstSpan) -> Result<ConstantDefinition, CodegenError> {
    // Get the first `ConstantDefinition` that matches the constant's name
    let constants = contract.constants.lock().map_err(|_| CodegenError::new(CodegenErrorKind::LockingError, AstSpan(vec![]), None))?;
    let constant = if let Some(m) = constants.iter().find(|const_def| const_def.name.eq(&name)) {
        m.clone()
    } else {
        tracing::error!(target: "codegen", "MISSING CONSTANT DEFINITION \"{}\"", name);

        return Err(CodegenError {
            kind: CodegenErrorKind::MissingConstantDefinition(name.to_string()),
            span: ir_byte_span.clone_box(),
            token: None,
        });
    };
    Ok(constant)
}
