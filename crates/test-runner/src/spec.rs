use crate::errors::{DecoratorError, DecoratorErrorKind, RunnerError};
use crate::outcome::{Expectations, ExpectedOutcome};
use alloy_primitives::{Address, B256, U256, hex, keccak256};
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
    /// Transaction and block fields of the test call that differ from the runner's
    pub env: EnvOverrides,
    /// How the test call must end, and its gas bounds
    pub expectations: Expectations,
}

/// Transaction and block fields a test's decorators override; `None` keeps the runner's value
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EnvOverrides {
    /// Sender of the test transaction, both `msg.sender` and `tx.origin`
    pub caller: Option<Address>,
    /// Gas limit of the test transaction
    pub gas_limit: Option<u64>,
    /// Gas price of the test transaction
    pub gas_price: Option<u128>,
    /// Block number
    pub block_number: Option<U256>,
    /// Block timestamp
    pub timestamp: Option<U256>,
    /// Block base fee
    pub basefee: Option<u64>,
    /// Blob base fee
    pub blob_basefee: Option<u128>,
    /// Block beneficiary
    pub coinbase: Option<Address>,
    /// Chain id
    pub chain_id: Option<u64>,
    /// Block `prevrandao`
    pub prevrandao: Option<B256>,
    /// Beacon slot number read by `slotnum`
    pub slot_number: Option<u64>,
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
    /// The runner's sender of the test transaction; a `caller` flag replaces it as `caller`
    pub caller: Address,
}

/// The decorator flags `hnc test` understands
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DecoratorKind {
    /// `calldata(bytes...)`: the calldata of the test transaction
    Calldata,
    /// `value(word)`: the call value of the test transaction
    Value,
    /// `store(slot, value, at = address)`: a storage slot set before the test runs
    Store,
    /// `caller(address)`: the sender of the test, both `caller` and `origin`
    Caller,
    /// `coinbase(address)`: the block beneficiary
    Coinbase,
    /// `gas_limit(n)`: the gas limit of the test transaction
    GasLimit,
    /// `gas_price(n)`: the gas price of the test transaction
    GasPrice,
    /// `block_number(n)`: the block number
    BlockNumber,
    /// `timestamp(n)`: the block timestamp
    Timestamp,
    /// `basefee(n)`: the block base fee
    Basefee,
    /// `blob_basefee(n)`: the blob base fee
    BlobBasefee,
    /// `chain_id(n)`: the chain id
    ChainId,
    /// `prevrandao(word)`: the block `prevrandao`
    Prevrandao,
    /// `slot_number(n)`: the beacon slot number read by `slotnum`
    SlotNumber,
    /// A flag that says how the test must end
    Outcome(OutcomeFlag),
    /// `expect_gas(min = n, max = m)`: bounds of the gas the test uses
    ExpectGas,
}

/// The flags that say how the test must end; a test can have only one of them
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum OutcomeFlag {
    /// `expect_revert` or `expect_revert(bytes...)`: the test reverts, with exactly this data when given
    Revert,
    /// `expect_partial_revert(bytes...)`: the test reverts with data that starts with this
    PartialRevert,
    /// `expect_halt` or `expect_halt("Reason")`: the test halts exceptionally, for this reason when given
    Halt,
    /// `expect_return(bytes...)`: the test returns exactly this data
    Return,
}

impl DecoratorKind {
    /// Every flag with its name in the source
    const NAMES: [(Self, &'static str); 19] = [
        (Self::Calldata, "calldata"),
        (Self::Value, "value"),
        (Self::Store, "store"),
        (Self::Caller, "caller"),
        (Self::Coinbase, "coinbase"),
        (Self::GasLimit, "gas_limit"),
        (Self::GasPrice, "gas_price"),
        (Self::BlockNumber, "block_number"),
        (Self::Timestamp, "timestamp"),
        (Self::Basefee, "basefee"),
        (Self::BlobBasefee, "blob_basefee"),
        (Self::ChainId, "chain_id"),
        (Self::Prevrandao, "prevrandao"),
        (Self::SlotNumber, "slot_number"),
        (Self::Outcome(OutcomeFlag::Revert), "expect_revert"),
        (Self::Outcome(OutcomeFlag::PartialRevert), "expect_partial_revert"),
        (Self::Outcome(OutcomeFlag::Halt), "expect_halt"),
        (Self::Outcome(OutcomeFlag::Return), "expect_return"),
        (Self::ExpectGas, "expect_gas"),
    ];

    /// The flag with this name, or `None` for a name `hnc test` does not know
    fn from_name(name: &str) -> Option<Self> {
        Self::NAMES.iter().find(|(_, n)| *n == name).map(|(kind, _)| *kind)
    }

    /// Whether the flag may be given more than once in a test
    fn is_repeatable(self) -> bool {
        self == Self::Store
    }
}

impl TestSpec {
    /// Checks a test's decorator flags and evaluates their arguments into the setup the runner applies
    pub fn from_decorator(decorator: Option<&Decorator>, contract: &Contract, ctx: SpecContext) -> Result<Self, RunnerError> {
        let Some(decorator) = decorator else {
            return Ok(Self::default());
        };

        let mut spec = Self::default();
        let mut eval = Evaluator { contract, ctx };
        // `caller` in other values means the test's sender, wherever the `caller` flag appears
        if let Some(flag) = decorator.flags.iter().find(|f| DecoratorKind::from_name(&f.name) == Some(DecoratorKind::Caller))
            && let [arg] = flag.args.as_slice()
            && arg.key.is_none()
        {
            eval.ctx.caller = eval.address(flag, arg)?;
        }
        let mut single_use: HashMap<DecoratorKind, &DecoratorFlag> = HashMap::new();
        let mut stored_slots: HashMap<(Address, U256), &DecoratorFlag> = HashMap::new();
        let mut outcome_flag: Option<&DecoratorFlag> = None;

        for flag in &decorator.flags {
            let Some(kind) = DecoratorKind::from_name(&flag.name) else {
                return Err(decorator_error(
                    flag,
                    DecoratorErrorKind::UnknownFlag,
                    format!("Unknown decorator flag: {}", flag.name),
                    flag.span.clone(),
                ));
            };
            if !kind.is_repeatable() {
                check_single_use(&mut single_use, kind, flag)?;
            }

            match kind {
                DecoratorKind::Calldata => {
                    let args = FlagArgs::new(
                        flag,
                        1..=usize::MAX,
                        &[],
                        "`calldata` takes the calldata bytes, e.g. calldata(__FUNC_SIG(transfer), 0x01)",
                    )?;
                    spec.calldata = eval.bytes(flag, &args.positional)?;
                }
                DecoratorKind::Value => {
                    let arg = single_arg(flag, "a single call value, e.g. value(0x01)")?;
                    spec.value = eval.word(flag, arg)?;
                }
                DecoratorKind::Store => {
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
                DecoratorKind::Caller => {
                    let arg = single_arg(flag, "an address, e.g. caller(0xbeef)")?;
                    spec.env.caller = Some(eval.address(flag, arg)?);
                }
                DecoratorKind::Coinbase => {
                    let arg = single_arg(flag, "an address, e.g. coinbase(0xbeef)")?;
                    spec.env.coinbase = Some(eval.address(flag, arg)?);
                }
                DecoratorKind::GasLimit => {
                    let arg = single_arg(flag, "a gas amount, e.g. gas_limit(100000)")?;
                    spec.env.gas_limit = Some(eval.uint(flag, arg)?);
                }
                DecoratorKind::GasPrice => {
                    let arg = single_arg(flag, "a price in wei, e.g. gas_price(10)")?;
                    spec.env.gas_price = Some(eval.uint(flag, arg)?);
                }
                DecoratorKind::BlockNumber => {
                    let arg = single_arg(flag, "a number, e.g. block_number(100)")?;
                    spec.env.block_number = Some(eval.word(flag, arg)?);
                }
                DecoratorKind::Timestamp => {
                    let arg = single_arg(flag, "a unix time, e.g. timestamp(1700000000)")?;
                    spec.env.timestamp = Some(eval.word(flag, arg)?);
                }
                DecoratorKind::Basefee => {
                    let arg = single_arg(flag, "a fee in wei, e.g. basefee(7)")?;
                    spec.env.basefee = Some(eval.uint(flag, arg)?);
                }
                DecoratorKind::BlobBasefee => {
                    let arg = single_arg(flag, "a fee in wei, e.g. blob_basefee(1)")?;
                    spec.env.blob_basefee = Some(eval.uint(flag, arg)?);
                }
                DecoratorKind::ChainId => {
                    let arg = single_arg(flag, "a chain id, e.g. chain_id(1)")?;
                    spec.env.chain_id = Some(eval.uint(flag, arg)?);
                }
                DecoratorKind::Prevrandao => {
                    let arg = single_arg(flag, "a 32-byte value, e.g. prevrandao(0x01)")?;
                    spec.env.prevrandao = Some(eval.word(flag, arg)?.into());
                }
                DecoratorKind::SlotNumber => {
                    let arg = single_arg(flag, "a slot, e.g. slot_number(100)")?;
                    spec.env.slot_number = Some(eval.uint(flag, arg)?);
                }
                DecoratorKind::Outcome(outcome) => {
                    if let Some(first) = outcome_flag {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::Conflict,
                            format!("`{}` conflicts with `{}`: a test can expect only one way to end", flag.name, first.name),
                            joined_span(first, flag),
                        ));
                    }
                    outcome_flag = Some(flag);
                    spec.expectations.outcome = eval.expected_outcome(flag, outcome)?;
                    spec.expectations.outcome_flag = Some(flag.name.clone());
                }
                DecoratorKind::ExpectGas => {
                    let usage = "`expect_gas` takes `min =` and/or `max =`, e.g. expect_gas(max = 30000)";
                    let args = FlagArgs::new(flag, 0..=0, &["min", "max"], usage)?;
                    if args.named.is_empty() {
                        return Err(decorator_error(flag, DecoratorErrorKind::InvalidArguments, usage, flag.span.clone()));
                    }
                    let min = args.named.get("min").map(|arg| eval.uint::<u64>(flag, arg)).transpose()?;
                    let max = args.named.get("max").map(|arg| eval.uint::<u64>(flag, arg)).transpose()?;
                    if let (Some(min), Some(max)) = (min, max)
                        && min > max
                    {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::InvalidValue,
                            format!("`min` ({min}) is larger than `max` ({max})"),
                            flag.span.clone(),
                        ));
                    }
                    spec.expectations.min_gas = min;
                    spec.expectations.max_gas = max;
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

    /// How an outcome flag (`expect_revert`, `expect_partial_revert`, `expect_halt`, `expect_return`) expects the test to end
    fn expected_outcome(&self, flag: &DecoratorFlag, outcome: OutcomeFlag) -> Result<ExpectedOutcome, RunnerError> {
        let outcome = match outcome {
            OutcomeFlag::Revert => {
                let usage = "`expect_revert` takes nothing, or the exact revert data, e.g. expect_revert(__ERROR(Unauthorized))";
                let args = FlagArgs::new(flag, 0..=usize::MAX, &[], usage)?;
                ExpectedOutcome::Revert(self.optional_bytes(flag, &args.positional)?)
            }
            OutcomeFlag::PartialRevert => {
                let usage = "`expect_partial_revert` takes the start of the revert data, e.g. expect_partial_revert(__ERROR(Unauthorized))";
                let args = FlagArgs::new(flag, 1..=usize::MAX, &[], usage)?;
                ExpectedOutcome::PartialRevert(self.bytes(flag, &args.positional)?)
            }
            OutcomeFlag::Halt => {
                let usage = "`expect_halt` takes nothing, or a halt reason, e.g. expect_halt(\"OutOfGas\")";
                let args = FlagArgs::new(flag, 0..=1, &[], usage)?;
                let reason = match args.positional.first() {
                    None => None,
                    Some(DecoratorArg { value: DecoratorValue::Str(reason), .. }) => Some(reason.clone()),
                    Some(arg) => return Err(decorator_error(flag, DecoratorErrorKind::InvalidValue, usage, arg.span.clone())),
                };
                ExpectedOutcome::Halt(reason)
            }
            OutcomeFlag::Return => {
                let usage = "`expect_return` takes the exact return data, e.g. expect_return(0x01)";
                let args = FlagArgs::new(flag, 0..=usize::MAX, &[], usage)?;
                ExpectedOutcome::Return(Some(self.bytes(flag, &args.positional)?))
            }
        };
        Ok(outcome)
    }

    /// The bytes of a byte sequence, or `None` when there are no arguments
    fn optional_bytes(&self, flag: &DecoratorFlag, args: &[&DecoratorArg]) -> Result<Option<Vec<u8>>, RunnerError> {
        if args.is_empty() { Ok(None) } else { self.bytes(flag, args).map(Some) }
    }

    /// An argument that must fit in a smaller unsigned integer, such as a `u64` gas limit
    fn uint<T: TryFrom<U256>>(&self, flag: &DecoratorFlag, arg: &DecoratorArg) -> Result<T, RunnerError> {
        let word = self.word(flag, arg)?;
        T::try_from(word).map_err(|_| {
            decorator_error(flag, DecoratorErrorKind::InvalidValue, format!("{word:#x} is too large for `{}`", flag.name), arg.span.clone())
        })
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

/// The only argument of a flag that takes one value
fn single_arg<'a>(flag: &'a DecoratorFlag, takes: &str) -> Result<&'a DecoratorArg, RunnerError> {
    let args = FlagArgs::new(flag, 1..=1, &[], &format!("`{}` takes {takes}", flag.name))?;
    Ok(args.positional[0])
}

/// Fails when a flag that may appear only once was already given
fn check_single_use<'a>(
    seen: &mut HashMap<DecoratorKind, &'a DecoratorFlag>,
    kind: DecoratorKind,
    flag: &'a DecoratorFlag,
) -> Result<(), RunnerError> {
    match seen.insert(kind, flag) {
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

#[cfg(test)]
#[test]
fn test_decorator_names_map_to_their_kind() {
    let mut names = std::collections::HashSet::new();
    for (kind, name) in DecoratorKind::NAMES {
        assert!(names.insert(name), "`{name}` is listed twice");
        assert_eq!(DecoratorKind::from_name(name), Some(kind), "{name}");
    }
    assert_eq!(DecoratorKind::from_name("origin"), None);
}
