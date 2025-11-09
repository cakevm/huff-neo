use crate::Codegen;
use alloy_primitives::U256;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::prelude::{CodegenError, CodegenErrorKind, ConstVal, ConstantDefinition, Contract, PushValue, str_to_bytes32};

/// Transforms a constant definition into a PushValue
/// Returns None for __NOOP constants (which generate no bytecode)
pub fn constant_gen(name: &str, contract: &Contract, ir_byte_span: &AstSpan) -> Result<Option<PushValue>, CodegenError> {
    let constant = lookup_constant(name, contract, ir_byte_span)?;

    // Generate bytecode for the constant
    // Should always be a `Literal` if storage pointers were derived in the AST
    // prior to generating the IR bytes.
    tracing::info!(target: "codegen", "FOUND CONSTANT DEFINITION: {}", constant.name);
    let push_value = match &constant.value {
        ConstVal::Bytes(bytes) => {
            let hex_literal = str_to_bytes32(&bytes.as_str());
            Some(PushValue::from(hex_literal))
        }
        ConstVal::String(_s) => {
            // String constants cannot be used directly - they must be used with __BYTES
            tracing::error!(target: "codegen", "STRING CONSTANT \"{}\" CANNOT BE USED DIRECTLY", constant.name);
            return Err(CodegenError {
                kind: CodegenErrorKind::StringConstantNotBytes(format!(
                    "String constant [{}] cannot be pushed directly. Use __BYTES([{}]) to convert it to bytes.",
                    name, name
                )),
                span: ir_byte_span.clone_box(),
                token: None,
            });
        }
        ConstVal::StoragePointer(sp) => Some(PushValue::from(sp)),
        ConstVal::BuiltinFunctionCall(bf) => Some(Codegen::gen_builtin_bytecode(contract, bf, ir_byte_span.clone())?),
        ConstVal::Expression(expr) => {
            tracing::info!(target: "codegen", "EVALUATING CONSTANT EXPRESSION FOR \"{}\"", constant.name);
            let evaluated = contract.evaluate_constant_expression(expr)?;
            Some(PushValue::from(evaluated))
        }
        ConstVal::Noop => {
            tracing::info!(target: "codegen", "CONSTANT \"{}\" IS __NOOP - GENERATING NO BYTECODE", constant.name);
            None // Generate no bytecode
        }
        ConstVal::FreeStoragePointer(fsp) => {
            // If this is reached in codegen stage, the `derive_storage_pointers`
            // method was not called on the AST.
            tracing::error!(target: "codegen", "STORAGE POINTERS INCORRECTLY DERIVED FOR \"{:?}\"", fsp);
            return Err(CodegenError { kind: CodegenErrorKind::StoragePointersNotDerived, span: constant.span.clone_box(), token: None });
        }
    };

    Ok(push_value)
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

/// Evaluates a constant to its numeric value as a usize
///
/// This is useful for builtin functions that need numeric constant values
/// (e.g., __ASSERT_PC) without generating bytecode.
///
/// Returns an error if:
/// - The constant doesn't exist
/// - The constant is __NOOP (has no numeric value)
/// - The constant is a builtin function call
/// - The constant value exceeds usize::MAX
pub fn evaluate_constant_value(name: &str, contract: &Contract, span: &AstSpan) -> Result<usize, CodegenError> {
    let constant = lookup_constant(name, contract, span)?;

    let literal = match &constant.value {
        ConstVal::Bytes(bytes) => str_to_bytes32(&bytes.as_str()),
        ConstVal::Expression(expr) => {
            tracing::info!(target: "codegen", "EVALUATING CONSTANT EXPRESSION \"{}\" FOR NUMERIC VALUE", constant.name);
            contract.evaluate_constant_expression(expr)?
        }
        ConstVal::StoragePointer(lit) => *lit,
        ConstVal::String(_) => {
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidArguments(format!(
                    "Constant [{}] is a string which cannot be evaluated to a numeric value",
                    name
                )),
                span: span.clone_box(),
                token: None,
            });
        }
        ConstVal::Noop => {
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidArguments(format!(
                    "Constant [{}] is __NOOP which has no numeric value and cannot be used as a bytecode position",
                    name
                )),
                span: span.clone_box(),
                token: None,
            });
        }
        ConstVal::BuiltinFunctionCall(bf) => {
            return Err(CodegenError {
                kind: CodegenErrorKind::InvalidArguments(format!(
                    "Constant [{}] is a builtin function call ({}) which cannot be evaluated to a numeric position. \
                     Only literal values, arithmetic expressions, and storage pointers are supported for __ASSERT_PC",
                    name, bf.kind
                )),
                span: span.clone_box(),
                token: None,
            });
        }
        ConstVal::FreeStoragePointer(_) => {
            return Err(CodegenError { kind: CodegenErrorKind::StoragePointersNotDerived, span: span.clone_box(), token: None });
        }
    };

    // Convert [u8; 32] to U256, then to usize
    let value = U256::from_be_bytes(literal);

    // Check if value fits in usize
    // Note: usize is sufficient for bytecode positions because in realistic scenarios,
    // EVM contract bytecode size is limited by the block gas limit and the 24KB contract
    // size limit (EIP-170).
    if value > U256::from(usize::MAX) {
        return Err(CodegenError {
            kind: CodegenErrorKind::InvalidArguments(format!(
                "Constant [{}] value ({}) exceeds maximum bytecode position size ({}). \
                 Bytecode positions must fit within a usize for the target platform",
                name,
                value,
                usize::MAX
            )),
            span: span.clone_box(),
            token: None,
        });
    }

    Ok(value.to::<usize>())
}
