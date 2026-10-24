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
    pub storage: Vec<StorageSlot>,
    /// Balances set before the test call
    pub balances: Vec<AccountValue<U256>>,
    /// Code placed at addresses before the test call
    pub code: Vec<AccountValue<Vec<u8>>>,
    /// Nonces set before the test call
    pub nonces: Vec<AccountValue<u64>>,
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

/// A storage slot of an account with its value, set before the test or expected after it
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StorageSlot {
    /// The account that owns the slot
    pub address: Address,
    /// The storage slot
    pub slot: U256,
    /// The value of the slot
    pub value: U256,
}

/// A property of an account, such as its balance, set before the test or expected after it
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccountValue<T> {
    /// The account
    pub address: Address,
    /// The value of the property
    pub value: T,
}

/// Addresses that decorator arguments can refer to by name
#[derive(Debug, Clone, Copy)]
pub struct SpecContext {
    /// The address of the deployed test, referred to as `this`
    pub this: Address,
    /// The runner's sender of the test transaction; a `caller` flag replaces it as `caller`
    pub caller: Address,
}

/// The value an `expect_balance` or `expect_storage` flag expects after the test
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpectedValue {
    /// Exactly this value
    Exact(U256),
    /// A value from `min` to `max`, both inclusive and each optional
    Range { min: Option<U256>, max: Option<U256> },
}

impl ExpectedValue {
    /// Whether the value meets the expectation
    pub fn matches(&self, value: U256) -> bool {
        match *self {
            Self::Exact(expected) => value == expected,
            Self::Range { min, max } => min.is_none_or(|min| value >= min) && max.is_none_or(|max| value <= max),
        }
    }

    /// The expectation in words, with each value written by `show`, e.g. "at least 5 and at most 9"
    pub fn describe(&self, show: impl Fn(U256) -> String) -> String {
        match *self {
            Self::Exact(expected) => show(expected),
            Self::Range { min: Some(min), max: Some(max) } => format!("at least {} and at most {}", show(min), show(max)),
            Self::Range { min: Some(min), max: None } => format!("at least {}", show(min)),
            Self::Range { min: None, max: Some(max) } => format!("at most {}", show(max)),
            Self::Range { min: None, max: None } => String::from("any value"),
        }
    }
}

/// A storage slot of an account and the value it must hold after the test
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedSlot {
    /// The account that owns the slot
    pub address: Address,
    /// The storage slot
    pub slot: U256,
    /// The value the slot must hold
    pub expected: ExpectedValue,
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
    /// `deal(address, wei)`: a balance set before the test runs
    Deal,
    /// `etch(address, bytes...)`: code placed at an address before the test runs
    Etch,
    /// `nonce(address, n)`: a nonce set before the test runs
    Nonce,
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
    /// `expect_storage(slot, value, at = address)` or `expect_storage(slot, min = n, max = m)`: a storage slot after the test
    ExpectStorage,
    /// `expect_balance(address, wei)` or `expect_balance(address, min = n, max = m)`: a balance after the test
    ExpectBalance,
    /// `skip` or `skip("reason")`: the test is not run
    Skip,
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
    const NAMES: [(Self, &'static str); 25] = [
        (Self::Calldata, "calldata"),
        (Self::Value, "value"),
        (Self::Store, "store"),
        (Self::Deal, "deal"),
        (Self::Etch, "etch"),
        (Self::Nonce, "nonce"),
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
        (Self::ExpectStorage, "expect_storage"),
        (Self::ExpectBalance, "expect_balance"),
        (Self::Skip, "skip"),
    ];

    /// The flag with this name, or `None` for a name `hnc test` does not know
    fn from_name(name: &str) -> Option<Self> {
        Self::NAMES.iter().find(|(_, n)| *n == name).map(|(kind, _)| *kind)
    }

    /// Whether the flag may be given more than once in a test
    fn is_repeatable(self) -> bool {
        matches!(self, Self::Store | Self::Deal | Self::Etch | Self::Nonce | Self::ExpectStorage | Self::ExpectBalance)
    }

    /// Whether the flag checks state after the test, which a revert or halt rolls back
    fn checks_post_state(self) -> bool {
        matches!(self, Self::ExpectStorage | Self::ExpectBalance)
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
        let mut expected_slots: HashMap<(Address, U256), &DecoratorFlag> = HashMap::new();
        // Accounts set or checked per flag, so each flag may name an address only once
        let mut accounts: HashMap<(DecoratorKind, Address), &DecoratorFlag> = HashMap::new();
        let mut outcome_flag: Option<(&DecoratorFlag, OutcomeFlag)> = None;
        let mut post_state_flag: Option<&DecoratorFlag> = None;

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
            if kind.checks_post_state() {
                post_state_flag.get_or_insert(flag);
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
                    let address = eval.at_address(flag, &args)?;

                    if let Some(first) = stored_slots.insert((address, slot), flag) {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::Duplicate,
                            format!("Storage slot {slot:#x} at {address} is already set by an earlier `store`"),
                            joined_span(first, flag),
                        ));
                    }
                    spec.storage.push(StorageSlot { address, slot, value });
                }
                DecoratorKind::Deal => {
                    let usage = "`deal` takes an address and a balance in wei, e.g. deal(caller, 1000)";
                    let args = FlagArgs::new(flag, 2..=2, &[], usage)?;
                    let address = eval.address(flag, args.positional[0])?;
                    check_account_once(&mut accounts, kind, address, flag)?;
                    let value = eval.word(flag, args.positional[1])?;
                    spec.balances.push(AccountValue { address, value });
                }
                DecoratorKind::Etch => {
                    let usage = "`etch` takes an address and the code bytes, e.g. etch(0x1000, \"0x5f5ff3\")";
                    let args = FlagArgs::new(flag, 2..=usize::MAX, &[], usage)?;
                    let address = eval.address(flag, args.positional[0])?;
                    let replaced = if address == eval.ctx.this {
                        Some("the test itself")
                    } else if address == eval.ctx.caller {
                        Some("the sender of the test, which may not have code")
                    } else {
                        None
                    };
                    if let Some(replaced) = replaced {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::InvalidValue,
                            format!("`etch` cannot replace the code of {replaced}"),
                            args.positional[0].span.clone(),
                        ));
                    }
                    check_account_once(&mut accounts, kind, address, flag)?;
                    let code = eval.bytes(flag, &args.positional[1..])?;
                    spec.code.push(AccountValue { address, value: code });
                }
                DecoratorKind::Nonce => {
                    let usage = "`nonce` takes an address and a nonce, e.g. nonce(caller, 5)";
                    let args = FlagArgs::new(flag, 2..=2, &[], usage)?;
                    let address = eval.address(flag, args.positional[0])?;
                    check_account_once(&mut accounts, kind, address, flag)?;
                    let value = eval.uint(flag, args.positional[1])?;
                    spec.nonces.push(AccountValue { address, value });
                }
                DecoratorKind::ExpectStorage => {
                    let usage = "`expect_storage` takes a slot, then a value or bounds `min =` and/or `max =`, and an optional \
                                 `at = address`, e.g. expect_storage([SLOT], 0x01) or expect_storage([SLOT], max = 10, at = 0x1000)";
                    let args = FlagArgs::new(flag, 1..=2, &["at", "min", "max"], usage)?;
                    let slot = eval.word(flag, args.positional[0])?;
                    let expected = eval.expected_value(flag, &args, usage)?;
                    let address = eval.at_address(flag, &args)?;

                    if let Some(first) = expected_slots.insert((address, slot), flag) {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::Duplicate,
                            format!("Storage slot {slot:#x} at {address} is already checked by an earlier `expect_storage`"),
                            joined_span(first, flag),
                        ));
                    }
                    spec.expectations.storage.push(ExpectedSlot { address, slot, expected });
                }
                DecoratorKind::ExpectBalance => {
                    let usage = "`expect_balance` takes an address, then a balance in wei or bounds `min =` and/or `max =`, \
                                 e.g. expect_balance(this, 1000) or expect_balance(caller, min = 1, max = 1000)";
                    let args = FlagArgs::new(flag, 1..=2, &["min", "max"], usage)?;
                    let address = eval.address(flag, args.positional[0])?;
                    check_account_once(&mut accounts, kind, address, flag)?;
                    let expected = eval.expected_value(flag, &args, usage)?;
                    spec.expectations.balances.push(AccountValue { address, value: expected });
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
                    if let Some((first, _)) = outcome_flag {
                        return Err(decorator_error(
                            flag,
                            DecoratorErrorKind::Conflict,
                            format!("`{}` conflicts with `{}`: a test can expect only one way to end", flag.name, first.name),
                            joined_span(first, flag),
                        ));
                    }
                    outcome_flag = Some((flag, outcome));
                    spec.expectations.outcome = eval.expected_outcome(flag, outcome)?;
                    spec.expectations.outcome_flag = Some(flag.name.clone());
                }
                // The runner skips the test before it gets here, see `skipped`
                DecoratorKind::Skip => {}
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

        // A revert or halt rolls the state back, so there is nothing left to check
        if let (Some((outcome, kind)), Some(post_state)) = (outcome_flag, post_state_flag)
            && kind != OutcomeFlag::Return
        {
            return Err(decorator_error(
                post_state,
                DecoratorErrorKind::Conflict,
                format!("`{}` conflicts with `{}`: a reverted or halted test leaves no state to check", post_state.name, outcome.name),
                joined_span(outcome, post_state),
            ));
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

    /// The account in a flag's `at =` argument, or the test itself without one
    fn at_address(&self, flag: &DecoratorFlag, args: &FlagArgs) -> Result<Address, RunnerError> {
        match args.named.get("at") {
            Some(at) => self.address(flag, at),
            None => Ok(self.ctx.this),
        }
    }

    /// The exact value in a flag's second argument, or the inclusive range of its `min =` and `max =` arguments
    fn expected_value(&self, flag: &DecoratorFlag, args: &FlagArgs, usage: &str) -> Result<ExpectedValue, RunnerError> {
        let (min, max) = (args.named.get("min"), args.named.get("max"));
        match (args.positional.get(1), min, max) {
            (Some(exact), None, None) => Ok(ExpectedValue::Exact(self.word(flag, exact)?)),
            (None, min, max) if min.is_some() || max.is_some() => {
                let min = min.map(|arg| self.word(flag, arg)).transpose()?;
                let max = max.map(|arg| self.word(flag, arg)).transpose()?;
                if let (Some(min), Some(max)) = (min, max)
                    && min > max
                {
                    return Err(decorator_error(
                        flag,
                        DecoratorErrorKind::InvalidValue,
                        format!("`min` ({min:#x}) is larger than `max` ({max:#x})"),
                        flag.span.clone(),
                    ));
                }
                Ok(ExpectedValue::Range { min, max })
            }
            _ => Err(decorator_error(flag, DecoratorErrorKind::InvalidArguments, usage, flag.span.clone())),
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

/// The reason a `skip` flag gives, if any
fn skip_reason(flag: &DecoratorFlag) -> Result<Option<String>, RunnerError> {
    let usage = "`skip` takes nothing, or a reason, e.g. skip(\"needs a mainnet fork\")";
    let args = FlagArgs::new(flag, 0..=1, &[], usage)?;
    match args.positional.first() {
        None => Ok(None),
        Some(DecoratorArg { value: DecoratorValue::Str(reason), .. }) => Ok(Some(reason.clone())),
        Some(arg) => Err(decorator_error(flag, DecoratorErrorKind::InvalidValue, usage, arg.span.clone())),
    }
}

/// Whether a test's decorator skips it, with the reason when given. The runner checks this before
/// it compiles the test, so a skipped test may be unfinished.
pub fn skipped(decorator: Option<&Decorator>) -> Result<Option<Option<String>>, RunnerError> {
    decorator
        .and_then(|decorator| decorator.flags.iter().find(|flag| DecoratorKind::from_name(&flag.name) == Some(DecoratorKind::Skip)))
        .map(skip_reason)
        .transpose()
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

/// Fails when a flag that sets or checks an account was already given for the same address
fn check_account_once<'a>(
    seen: &mut HashMap<(DecoratorKind, Address), &'a DecoratorFlag>,
    kind: DecoratorKind,
    address: Address,
    flag: &'a DecoratorFlag,
) -> Result<(), RunnerError> {
    match seen.insert((kind, address), flag) {
        Some(first) => Err(decorator_error(
            flag,
            DecoratorErrorKind::Duplicate,
            format!("`{}` is already given for {address}", flag.name),
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
