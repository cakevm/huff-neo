use crate::errors::{DecoratorError, DecoratorErrorKind, RunnerError};
use alloy_primitives::{Address, U256, hex, keccak256};
use huff_neo_codegen::Codegen;
use huff_neo_utils::ast::huff::{
    BuiltinFunctionArg, BuiltinFunctionCall, BuiltinFunctionKind, Decorator, DecoratorArg, DecoratorFlag, DecoratorValue,
};
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::prelude::Contract;
use std::collections::HashMap;
use std::ops::RangeInclusive;

/// The setup a test's decorators ask for before the test call
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TestSpec {
    /// The call value of the test transaction
    pub value: U256,
    /// The calldata of the test transaction
    pub calldata: Vec<u8>,
    /// Storage slots written before the test call, in source order
    pub storage: Vec<StorageWrite>,
}

/// A storage slot set by a `store` decorator flag
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StorageWrite {
    /// The account whose storage is written
    pub address: Address,
    /// The storage slot
    pub slot: U256,
    /// The value written to the slot
    pub value: U256,
}

/// Addresses that decorator arguments can refer to by name
#[derive(Debug, Clone, Copy)]
pub struct SpecContext {
    /// The address of the deployed test, referred to as `this`
    pub this: Address,
    /// The sender of the test transaction, referred to as `caller`
    pub caller: Address,
}

impl TestSpec {
    /// Checks a test's decorator flags and evaluates their arguments into the setup the runner applies
    pub fn from_decorator(decorator: Option<&Decorator>, contract: &Contract, ctx: SpecContext) -> Result<Self, RunnerError> {
        let Some(decorator) = decorator else {
            return Ok(Self::default());
        };

        let mut spec = Self::default();
        let eval = Evaluator { contract, ctx };
        let mut single_use: HashMap<&str, &DecoratorFlag> = HashMap::new();
        let mut stored_slots: HashMap<(Address, U256), &DecoratorFlag> = HashMap::new();

        for flag in &decorator.flags {
            match flag.name.as_str() {
                "calldata" => {
                    check_single_use(&mut single_use, flag)?;
                    let args = FlagArgs::new(
                        flag,
                        1..=usize::MAX,
                        &[],
                        "`calldata` takes the calldata bytes, e.g. calldata(__FUNC_SIG(transfer), 0x01)",
                    )?;
                    spec.calldata = eval.bytes(flag, &args.positional)?;
                }
                "value" => {
                    check_single_use(&mut single_use, flag)?;
                    let args = FlagArgs::new(flag, 1..=1, &[], "`value` takes a single call value, e.g. value(0x01)")?;
                    spec.value = eval.word(flag, args.positional[0])?;
                }
                "store" => {
                    let args = FlagArgs::new(
                        flag,
                        2..=2,
                        &["at"],
                        "`store` takes a slot, a value and an optional `at = address`, e.g. store([SLOT], 0x01, at = 0x1000)",
                    )?;
                    let slot = eval.word(flag, args.positional[0])?;
                    let value = eval.word(flag, args.positional[1])?;
                    let address = match args.named.get("at") {
                        Some(at) => eval.address(flag, at)?,
                        None => ctx.this,
                    };

                    if let Some(first) = stored_slots.insert((address, slot), flag) {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::Duplicate,
                            format!("Storage slot {slot:#x} at {address} is already set by an earlier `store`"),
                            joined_span(first, flag),
                        ));
                    }
                    spec.storage.push(StorageWrite { address, slot, value });
                }
                name => {
                    return Err(decorator_error(
                        flag,
                        DecoratorErrorKind::UnknownFlag,
                        format!("Unknown decorator flag: {name}"),
                        flag.span.clone(),
                    ));
                }
            }
        }

        Ok(spec)
    }
}

/// The arguments of a flag, checked against the positional count and keys it accepts
struct FlagArgs<'a> {
    positional: Vec<&'a DecoratorArg>,
    named: HashMap<&'a str, &'a DecoratorArg>,
}

impl<'a> FlagArgs<'a> {
    fn new(flag: &'a DecoratorFlag, positional: RangeInclusive<usize>, keys: &[&str], usage: &str) -> Result<Self, RunnerError> {
        let invalid = |hint: String| decorator_error(flag, DecoratorErrorKind::InvalidArguments, hint, flag.span.clone());

        let mut args = FlagArgs { positional: Vec::new(), named: HashMap::new() };
        for arg in &flag.args {
            match &arg.key {
                None => args.positional.push(arg),
                Some(key) if !keys.contains(&key.as_str()) => return Err(invalid(format!("Unknown argument `{key}`. {usage}"))),
                Some(key) => {
                    if args.named.insert(key, arg).is_some() {
                        return Err(invalid(format!("Argument `{key}` is given twice. {usage}")));
                    }
                }
            }
        }

        if !positional.contains(&args.positional.len()) {
            return Err(invalid(usage.to_string()));
        }
        Ok(args)
    }
}

/// Evaluates decorator argument values against the contract and the test's addresses
struct Evaluator<'a> {
    contract: &'a Contract,
    ctx: SpecContext,
}

impl Evaluator<'_> {
    /// The 32-byte word an argument stands for, the same value Huff would push for it
    fn word(&self, flag: &DecoratorFlag, arg: &DecoratorArg) -> Result<U256, RunnerError> {
        let invalid = |hint: String| decorator_error(flag, DecoratorErrorKind::InvalidValue, hint, arg.span.clone());

        match &arg.value {
            DecoratorValue::Hex(digits) if digits.len() > 64 => Err(invalid(String::from("A hex literal word is at most 32 bytes"))),
            DecoratorValue::Hex(digits) => Ok(U256::from_str_radix(digits, 16).expect("the lexer only produces hex digits")),
            DecoratorValue::Int(int) => Ok(U256::from(*int)),
            DecoratorValue::Bool(b) => Ok(U256::from(*b as u8)),
            DecoratorValue::Constant(name) => Codegen::gen_constant_value(self.contract, name, &arg.span)?
                .map(|v| U256::from_be_bytes(v.value.0))
                .ok_or_else(|| invalid(format!("Constant [{name}] is __NOOP and has no value"))),
            DecoratorValue::Builtin(bf) if bf.kind == BuiltinFunctionKind::Error => {
                let (selector, defined) = self.error_selector(flag, arg, bf)?;
                // A defined error's selector is pushed left-aligned, ready to be stored as revert data
                let mut word = [0u8; 32];
                if defined {
                    word[..4].copy_from_slice(&selector)
                } else {
                    word[28..].copy_from_slice(&selector)
                }
                Ok(U256::from_be_bytes(word))
            }
            DecoratorValue::Builtin(bf) => {
                let value = Codegen::gen_builtin_bytecode(self.contract, bf, arg.span.clone())?;
                Ok(U256::from_be_bytes(value.value.0))
            }
            DecoratorValue::Ident(ident) => match ident.as_str() {
                "this" => Ok(U256::from_be_slice(self.ctx.this.as_slice())),
                "caller" => Ok(U256::from_be_slice(self.ctx.caller.as_slice())),
                _ => Err(invalid(format!("Unknown identifier `{ident}`; expected `this` or `caller`"))),
            },
            DecoratorValue::Str(_) => {
                Err(invalid(String::from("Expected a hex or decimal literal, boolean, [CONSTANT], builtin call, `this` or `caller`")))
            }
        }
    }

    /// An argument that must be an address
    fn address(&self, flag: &DecoratorFlag, arg: &DecoratorArg) -> Result<Address, RunnerError> {
        let word = self.word(flag, arg)?;
        if word.bit_len() > Address::len_bytes() * 8 {
            return Err(decorator_error(flag, DecoratorErrorKind::InvalidValue, format!("{word:#x} is not an address"), arg.span.clone()));
        }
        Ok(Address::from_word(word.into()))
    }

    /// The bytes a sequence of arguments stands for: selectors add 4 bytes, hex keeps its written
    /// width, and every other value adds a 32-byte word
    fn bytes(&self, flag: &DecoratorFlag, args: &[&DecoratorArg]) -> Result<Vec<u8>, RunnerError> {
        let mut bytes = Vec::new();
        for arg in args {
            match &arg.value {
                DecoratorValue::Str(s) => {
                    let decoded = hex::decode(s.strip_prefix("0x").unwrap_or(s)).map_err(|_| {
                        decorator_error(flag, DecoratorErrorKind::InvalidValue, format!("\"{s}\" is not a hex string"), arg.span.clone())
                    })?;
                    bytes.extend(decoded);
                }
                DecoratorValue::Hex(digits) => {
                    // An odd number of digits is padded with a leading zero nibble
                    let decoded = if digits.len() % 2 == 1 { hex::decode(format!("0{digits}")) } else { hex::decode(digits) };
                    bytes.extend(decoded.expect("the lexer only produces hex digits"));
                }
                DecoratorValue::Builtin(bf) if bf.kind == BuiltinFunctionKind::FunctionSignature => {
                    let word = self.word(flag, arg)?.to_be_bytes::<32>();
                    bytes.extend(&word[28..]);
                }
                DecoratorValue::Builtin(bf) if bf.kind == BuiltinFunctionKind::Error => {
                    bytes.extend(self.error_selector(flag, arg, bf)?.0);
                }
                _ => bytes.extend(self.word(flag, arg)?.to_be_bytes::<32>()),
            }
        }
        Ok(bytes)
    }

    /// The selector of `__ERROR(..)`, and whether it names an error defined in the contract
    fn error_selector(&self, flag: &DecoratorFlag, arg: &DecoratorArg, bf: &BuiltinFunctionCall) -> Result<([u8; 4], bool), RunnerError> {
        let name = match bf.args.as_slice() {
            [BuiltinFunctionArg::StringLiteral(s, _) | BuiltinFunctionArg::Identifier(s, _)] => s,
            _ => {
                return Err(decorator_error(
                    flag,
                    DecoratorErrorKind::InvalidValue,
                    "__ERROR takes an error name or signature",
                    arg.span.clone(),
                ));
            }
        };
        Ok(match self.contract.errors.iter().find(|e| &e.name == name) {
            Some(error) => (error.selector, true),
            None => (keccak256(name)[..4].try_into().expect("a selector is 4 bytes"), false),
        })
    }
}

/// Fails when a flag that may appear only once was already given
fn check_single_use<'a>(seen: &mut HashMap<&'a str, &'a DecoratorFlag>, flag: &'a DecoratorFlag) -> Result<(), RunnerError> {
    match seen.insert(flag.name.as_str(), flag) {
        Some(first) => Err(decorator_error(
            flag,
            DecoratorErrorKind::Duplicate,
            format!("`{}` may only be given once", flag.name),
            joined_span(first, flag),
        )),
        None => Ok(()),
    }
}

/// The source of two flags, so a duplicate points at both places
fn joined_span(first: &DecoratorFlag, second: &DecoratorFlag) -> AstSpan {
    AstSpan(first.span.0.iter().chain(&second.span.0).cloned().collect())
}

/// An error pointing at a decorator flag the runner cannot apply
fn decorator_error(flag: &DecoratorFlag, kind: DecoratorErrorKind, hint: impl Into<String>, span: AstSpan) -> RunnerError {
    RunnerError::InvalidDecorator(DecoratorError { flag: flag.name.clone(), kind, hint: hint.into(), span })
}
