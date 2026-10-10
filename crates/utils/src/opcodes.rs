use alloy_primitives::{U256, hex};
use phf::phf_map;
use std::fmt;
use strum_macros::{AsRefStr, EnumString};

use crate::evm_version::SupportedEVMVersions;

/// All the EVM opcodes as a static array
/// They are arranged in a particular order such that all the opcodes that have common
/// prefixes are ordered alphabetically by decreasing length to avoid mismatch when lexing.
/// Example : [origin, or] or [push32, ..., push3]
pub const OPCODES: [&str; 156] = [
    "addmod",
    "address",
    "add",
    "and",
    "balance",
    "basefee",
    "blobbasefee",
    "blobhash",
    "blockhash",
    "byte",
    "callcode",
    "calldatacopy",
    "calldataload",
    "calldatasize",
    "caller",
    "callvalue",
    "call",
    "chainid",
    "clz",
    "codecopy",
    "codesize",
    "coinbase",
    "create2",
    "create",
    "delegatecall",
    "difficulty",
    "div",
    "dup10",
    "dup11",
    "dup12",
    "dup13",
    "dup14",
    "dup15",
    "dup16",
    "dup1",
    "dup2",
    "dup3",
    "dup4",
    "dup5",
    "dup6",
    "dup7",
    "dup8",
    "dup9",
    "dupn",
    "eq",
    "exchange",
    "exp",
    "extcodecopy",
    "extcodehash",
    "extcodesize",
    "gaslimit",
    "gasprice",
    "gas",
    "gt",
    "invalid",
    "iszero",
    "jumpdest",
    "jumpi",
    "jump",
    "keccak256",
    "log0",
    "log1",
    "log2",
    "log3",
    "log4",
    "lt",
    "mcopy",
    "mload",
    "mod",
    "msize",
    "mstore8",
    "mstore",
    "mulmod",
    "mul",
    "not",
    "number",
    "origin",
    "or",
    "pc",
    "pop",
    "prevrandao",
    "push0",
    "push10",
    "push11",
    "push12",
    "push13",
    "push14",
    "push15",
    "push16",
    "push17",
    "push18",
    "push19",
    "push1",
    "push20",
    "push21",
    "push22",
    "push23",
    "push24",
    "push25",
    "push26",
    "push27",
    "push28",
    "push29",
    "push2",
    "push30",
    "push31",
    "push32",
    "push3",
    "push4",
    "push5",
    "push6",
    "push7",
    "push8",
    "push9",
    "returndatacopy",
    "returndatasize",
    "return",
    "revert",
    "sar",
    "sdiv",
    "selfbalance",
    "selfdestruct",
    "sgt",
    "sha3",
    "shl",
    "shr",
    "signextend",
    "sload",
    "slotnum",
    "slt",
    "smod",
    "sstore",
    "staticcall",
    "stop",
    "sub",
    "swap10",
    "swap11",
    "swap12",
    "swap13",
    "swap14",
    "swap15",
    "swap16",
    "swap1",
    "swap2",
    "swap3",
    "swap4",
    "swap5",
    "swap6",
    "swap7",
    "swap8",
    "swap9",
    "swapn",
    "timestamp",
    "tload",
    "tstore",
    "xor",
];

/// Hashmap of all the EVM opcodes
pub static OPCODES_MAP: phf::Map<&'static str, Opcode> = phf_map! {
    "lt" => Opcode::Lt,
    "gt" => Opcode::Gt,
    "slt" => Opcode::Slt,
    "sgt" => Opcode::Sgt,
    "eq" => Opcode::Eq,
    "iszero" => Opcode::Iszero,
    "and" => Opcode::And,
    "or" => Opcode::Or,
    "xor" => Opcode::Xor,
    "not" => Opcode::Not,
    "sha3" => Opcode::Keccak256, // backward compatibility for huff-rs
    "keccak256" => Opcode::Keccak256,
    "address" => Opcode::Address,
    "balance" => Opcode::Balance,
    "origin" => Opcode::Origin,
    "caller" => Opcode::Caller,
    "callvalue" => Opcode::Callvalue,
    "calldataload" => Opcode::Calldataload,
    "calldatasize" => Opcode::Calldatasize,
    "calldatacopy" => Opcode::Calldatacopy,
    "codesize" => Opcode::Codesize,
    "codecopy" => Opcode::Codecopy,
    "basefee" => Opcode::Basefee,
    "blobhash" => Opcode::Blobhash,
    "blobbasefee" => Opcode::Blobbasefee,
    "blockhash" => Opcode::Blockhash,
    "coinbase" => Opcode::Coinbase,
    "timestamp" => Opcode::Timestamp,
    "number" => Opcode::Number,
    "difficulty" => Opcode::Difficulty,
    "prevrandao" => Opcode::Prevrandao,
    "gaslimit" => Opcode::Gaslimit,
    "chainid" => Opcode::Chainid,
    "clz" => Opcode::Clz,
    "selfbalance" => Opcode::Selfbalance,
    "slotnum" => Opcode::Slotnum,
    "pop" => Opcode::Pop,
    "mload" => Opcode::Mload,
    "mstore" => Opcode::Mstore,
    "mstore8" => Opcode::Mstore8,
    "sload" => Opcode::Sload,
    "sstore" => Opcode::Sstore,
    "jump" => Opcode::Jump,
    "jumpi" => Opcode::Jumpi,
    "pc" => Opcode::Pc,
    "msize" => Opcode::Msize,
    "mcopy" => Opcode::Mcopy,
    "push0" => Opcode::Push0,
    "push1" => Opcode::Push1,
    "push2" => Opcode::Push2,
    "push3" => Opcode::Push3,
    "push4" => Opcode::Push4,
    "push5" => Opcode::Push5,
    "push6" => Opcode::Push6,
    "push7" => Opcode::Push7,
    "push8" => Opcode::Push8,
    "push9" => Opcode::Push9,
    "push10" => Opcode::Push10,
    "push17" => Opcode::Push17,
    "push18" => Opcode::Push18,
    "push19" => Opcode::Push19,
    "push20" => Opcode::Push20,
    "push21" => Opcode::Push21,
    "push22" => Opcode::Push22,
    "push23" => Opcode::Push23,
    "push24" => Opcode::Push24,
    "push25" => Opcode::Push25,
    "push26" => Opcode::Push26,
    "dup1" => Opcode::Dup1,
    "dup2" => Opcode::Dup2,
    "dup3" => Opcode::Dup3,
    "dup4" => Opcode::Dup4,
    "dup5" => Opcode::Dup5,
    "dup6" => Opcode::Dup6,
    "dup7" => Opcode::Dup7,
    "dup8" => Opcode::Dup8,
    "dup9" => Opcode::Dup9,
    "dup10" => Opcode::Dup10,
    "swap1" => Opcode::Swap1,
    "swap2" => Opcode::Swap2,
    "swap3" => Opcode::Swap3,
    "swap4" => Opcode::Swap4,
    "swap5" => Opcode::Swap5,
    "swap6" => Opcode::Swap6,
    "swap7" => Opcode::Swap7,
    "swap8" => Opcode::Swap8,
    "swap9" => Opcode::Swap9,
    "swap10" => Opcode::Swap10,
    "stop" => Opcode::Stop,
    "add" => Opcode::Add,
    "mul" => Opcode::Mul,
    "sub" => Opcode::Sub,
    "div" => Opcode::Div,
    "sdiv" => Opcode::Sdiv,
    "mod" => Opcode::Mod,
    "smod" => Opcode::Smod,
    "addmod" => Opcode::Addmod,
    "mulmod" => Opcode::Mulmod,
    "exp" => Opcode::Exp,
    "signextend" => Opcode::Signextend,
    "byte" => Opcode::Byte,
    "shl" => Opcode::Shl,
    "shr" => Opcode::Shr,
    "sar" => Opcode::Sar,
    "gasprice" => Opcode::Gasprice,
    "extcodesize" => Opcode::Extcodesize,
    "extcodecopy" => Opcode::Extcodecopy,
    "returndatasize" => Opcode::Returndatasize,
    "returndatacopy" => Opcode::Returndatacopy,
    "extcodehash" => Opcode::Extcodehash,
    "gas" => Opcode::Gas,
    "jumpdest" => Opcode::Jumpdest,
    "push11" => Opcode::Push11,
    "push12" => Opcode::Push12,
    "push13" => Opcode::Push13,
    "push14" => Opcode::Push14,
    "push15" => Opcode::Push15,
    "push16" => Opcode::Push16,
    "push27" => Opcode::Push27,
    "push28" => Opcode::Push28,
    "push29" => Opcode::Push29,
    "push30" => Opcode::Push30,
    "push31" => Opcode::Push31,
    "push32" => Opcode::Push32,
    "dup11" => Opcode::Dup11,
    "dup12" => Opcode::Dup12,
    "dup13" => Opcode::Dup13,
    "dup14" => Opcode::Dup14,
    "dup15" => Opcode::Dup15,
    "dup16" => Opcode::Dup16,
    "dupn" => Opcode::Dupn,
    "swap11" => Opcode::Swap11,
    "swap12" => Opcode::Swap12,
    "swap13" => Opcode::Swap13,
    "swap14" => Opcode::Swap14,
    "swap15" => Opcode::Swap15,
    "swap16" => Opcode::Swap16,
    "swapn" => Opcode::Swapn,
    "exchange" => Opcode::Exchange,
    "log0" => Opcode::Log0,
    "log1" => Opcode::Log1,
    "log2" => Opcode::Log2,
    "log3" => Opcode::Log3,
    "log4" => Opcode::Log4,
    "tload" => Opcode::Tload,
    "tstore" => Opcode::Tstore,
    "create" => Opcode::Create,
    "call" => Opcode::Call,
    "callcode" => Opcode::Callcode,
    "return" => Opcode::Return,
    "delegatecall" => Opcode::Delegatecall,
    "staticcall" => Opcode::Staticcall,
    "create2" => Opcode::Create2,
    "revert" => Opcode::Revert,
    "invalid" => Opcode::Invalid,
    "selfdestruct" => Opcode::Selfdestruct,
};

/// EVM Opcodes
/// References <https://evm.codes> and <https://github.com/ethereum/execution-specs/blob/master/lists/evm/proposed-opcodes.md>
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumString, AsRefStr)]
#[strum(serialize_all = "lowercase")]
pub enum Opcode {
    /// Halts execution.
    Stop,
    /// Addition operation
    Add,
    /// Multiplication Operation
    Mul,
    /// Subtraction Operation
    Sub,
    /// Integer Division Operation
    Div,
    /// Signed Integer Division Operation
    Sdiv,
    /// Modulo Remainder Operation
    Mod,
    /// Signed Modulo Remainder Operation
    Smod,
    /// Modulo Addition Operation
    Addmod,
    /// Modulo Multiplication Operation
    Mulmod,
    /// Exponential Operation
    Exp,
    /// Extend Length of Two's Complement Signed Integer
    Signextend,
    /// Less-than Comparison
    Lt,
    /// Greater-than Comparison
    Gt,
    /// Signed Less-than Comparison
    Slt,
    /// Signed Greater-than Comparison
    Sgt,
    /// Equality Comparison
    Eq,
    /// Not Operation
    Iszero,
    /// Bitwise AND Operation
    And,
    /// Bitwise OR Operation
    Or,
    /// Bitwise XOR Operation
    Xor,
    /// Bitwise NOT Operation
    Not,
    /// Retrieve Single Byte from Word
    Byte,
    /// Left Shift Operation
    Shl,
    /// Right Shift Operation
    Shr,
    /// Arithmetic Shift Right Operation
    Sar,
    /// Compute the Keccak-256 hash of a 32-byte word
    #[strum(serialize = "keccak256", serialize = "sha3")]
    Keccak256,
    /// Address of currently executing account
    Address,
    /// Balance of a given account
    Balance,
    /// Address of execution origination
    Origin,
    /// Address of the caller
    Caller,
    /// Value of the call
    Callvalue,
    /// Loads Calldata
    Calldataload,
    /// Size of the Calldata
    Calldatasize,
    /// Copies the Calldata to Memory
    Calldatacopy,
    /// Size of the Executing Code
    Codesize,
    /// Copies Executing Code to Memory
    Codecopy,
    /// Current Price of Gas
    Gasprice,
    /// Size of an Account's Code
    Extcodesize,
    /// Copies an Account's Code to Memory
    Extcodecopy,
    /// Size of Output Data from Previous Call
    Returndatasize,
    /// Copies Output Data from Previous Call to Memory
    Returndatacopy,
    /// Hash of a Block from the most recent 256 blocks
    Blockhash,
    /// The Current Blocks Beneficiary Address
    Coinbase,
    /// The Current Blocks Timestamp
    Timestamp,
    /// The Current Blocks Number
    Number,
    /// The Current Blocks Difficulty
    Difficulty,
    /// Pseudorandomness from the Beacon Chain
    Prevrandao,
    /// The Current Blocks Gas Limit
    Gaslimit,
    /// The Chain ID
    Chainid,
    /// Count Leading Zeros - Returns the number of zeros preceding the most significant one bit
    Clz,
    /// Balance of the Currently Executing Account
    Selfbalance,
    /// Base Fee
    Basefee,
    /// Versioned hashes of blobs associated with the transaction.
    Blobhash,
    /// Blob base fee of the current block.
    Blobbasefee,
    /// Beacon chain slot number of the current block
    Slotnum,
    /// Removes an Item from the Stack
    Pop,
    /// Loads a word from Memory
    Mload,
    /// Stores a word in Memory
    Mstore,
    /// Stores a byte in Memory
    Mstore8,
    /// Load a word from Storage
    Sload,
    /// Store a word in Storage
    Sstore,
    /// Alter the Program Counter
    Jump,
    /// Conditionally Alter the Program Counter
    Jumpi,
    /// Value of the Program Counter Before the Current Instruction
    Pc,
    /// Size of Active Memory in Bytes
    Msize,
    /// Amount of available gas including the cost of the current instruction
    Gas,
    /// Marks a valid destination for jumps
    Jumpdest,
    /// Places a zero on top of the stack
    Push0,
    /// Places 1 byte item on top of the stack
    Push1,
    /// Places 2 byte item on top of the stack
    Push2,
    /// Places 3 byte item on top of the stack
    Push3,
    /// Places 4 byte item on top of the stack
    Push4,
    /// Places 5 byte item on top of the stack
    Push5,
    /// Places 6 byte item on top of the stack
    Push6,
    /// Places 7 byte item on top of the stack
    Push7,
    /// Places 8 byte item on top of the stack
    Push8,
    /// Places 9 byte item on top of the stack
    Push9,
    /// Places 10 byte item on top of the stack
    Push10,
    /// Places 11 byte item on top of the stack
    Push11,
    /// Places 12 byte item on top of the stack
    Push12,
    /// Places 13 byte item on top of the stack
    Push13,
    /// Places 14 byte item on top of the stack
    Push14,
    /// Places 15 byte item on top of the stack
    Push15,
    /// Places 16 byte item on top of the stack
    Push16,
    /// Places 17 byte item on top of the stack
    Push17,
    /// Places 18 byte item on top of the stack
    Push18,
    /// Places 19 byte item on top of the stack
    Push19,
    /// Places 20 byte item on top of the stack
    Push20,
    /// Places 21 byte item on top of the stack
    Push21,
    /// Places 22 byte item on top of the stack
    Push22,
    /// Places 23 byte item on top of the stack
    Push23,
    /// Places 24 byte item on top of the stack
    Push24,
    /// Places 25 byte item on top of the stack
    Push25,
    /// Places 26 byte item on top of the stack
    Push26,
    /// Places 27 byte item on top of the stack
    Push27,
    /// Places 28 byte item on top of the stack
    Push28,
    /// Places 29 byte item on top of the stack
    Push29,
    /// Places 30 byte item on top of the stack
    Push30,
    /// Places 31 byte item on top of the stack
    Push31,
    /// Places 32 byte item on top of the stack
    Push32,
    /// Duplicates the first stack item
    Dup1,
    /// Duplicates the 2nd stack item
    Dup2,
    /// Duplicates the 3rd stack item
    Dup3,
    /// Duplicates the 4th stack item
    Dup4,
    /// Duplicates the 5th stack item
    Dup5,
    /// Duplicates the 6th stack item
    Dup6,
    /// Duplicates the 7th stack item
    Dup7,
    /// Duplicates the 8th stack item
    Dup8,
    /// Duplicates the 9th stack item
    Dup9,
    /// Duplicates the 10th stack item
    Dup10,
    /// Duplicates the 11th stack item
    Dup11,
    /// Duplicates the 12th stack item
    Dup12,
    /// Duplicates the 13th stack item
    Dup13,
    /// Duplicates the 14th stack item
    Dup14,
    /// Duplicates the 15th stack item
    Dup15,
    /// Duplicates the 16th stack item
    Dup16,
    /// Duplicate the Nth stack item (17 <= N <= 235), with N given by an immediate byte
    Dupn,
    /// Exchange the top two stack items
    Swap1,
    /// Exchange the first and third stack items
    Swap2,
    /// Exchange the first and fourth stack items
    Swap3,
    /// Exchange the first and fifth stack items
    Swap4,
    /// Exchange the first and sixth stack items
    Swap5,
    /// Exchange the first and seventh stack items
    Swap6,
    /// Exchange the first and eighth stack items
    Swap7,
    /// Exchange the first and ninth stack items
    Swap8,
    /// Exchange the first and tenth stack items
    Swap9,
    /// Exchange the first and eleventh stack items
    Swap10,
    /// Exchange the first and twelfth stack items
    Swap11,
    /// Exchange the first and thirteenth stack items
    Swap12,
    /// Exchange the first and fourteenth stack items
    Swap13,
    /// Exchange the first and fifteenth stack items
    Swap14,
    /// Exchange the first and sixteenth stack items
    Swap15,
    /// Exchange the first and seventeenth stack items
    Swap16,
    /// Exchange the first and (N+1)th stack items (17 <= N <= 235), with N given by an immediate byte
    Swapn,
    /// Exchange the (N+1)th and (M+1)th stack items, with N and M given by an immediate byte
    Exchange,
    /// Append Log Record with no Topics
    Log0,
    /// Append Log Record with 1 Topic
    Log1,
    /// Append Log Record with 2 Topics
    Log2,
    /// Append Log Record with 3 Topics
    Log3,
    /// Append Log Record with 4 Topics
    Log4,
    /// Transaction-persistent, but storage-ephemeral variable load
    Tload,
    /// Transaction-persistent, but storage-ephemeral variable store
    Tstore,
    /// Copies an area of memory from src to dst. Areas can overlap.
    Mcopy,
    /// Create a new account with associated code
    Create,
    /// Message-call into an account
    Call,
    /// Message-call into this account with an alternative accounts code
    Callcode,
    /// Halt execution, returning output data
    Return,
    /// Message-call into this account with an alternative accounts code, persisting the sender and
    /// value
    Delegatecall,
    /// Create a new account with associated code
    Create2,
    /// Static Message-call into an account
    Staticcall,
    /// Halt execution, reverting state changes, but returning data and remaining gas
    Revert,
    /// Invalid Instruction
    Invalid,
    /// Halt Execution and Register Account for later deletion
    Selfdestruct,
    /// Get hash of an account’s code
    Extcodehash,
}

impl Opcode {
    /// Translates an Opcode into a string
    pub fn string(&self) -> String {
        let opcode_str = match self {
            Opcode::Stop => "00",
            Opcode::Add => "01",
            Opcode::Mul => "02",
            Opcode::Sub => "03",
            Opcode::Div => "04",
            Opcode::Sdiv => "05",
            Opcode::Mod => "06",
            Opcode::Smod => "07",
            Opcode::Addmod => "08",
            Opcode::Mulmod => "09",
            Opcode::Exp => "0a",
            Opcode::Signextend => "0b",
            Opcode::Lt => "10",
            Opcode::Gt => "11",
            Opcode::Slt => "12",
            Opcode::Sgt => "13",
            Opcode::Eq => "14",
            Opcode::Iszero => "15",
            Opcode::And => "16",
            Opcode::Or => "17",
            Opcode::Xor => "18",
            Opcode::Not => "19",
            Opcode::Byte => "1a",
            Opcode::Shl => "1b",
            Opcode::Shr => "1c",
            Opcode::Sar => "1d",
            Opcode::Keccak256 => "20",
            Opcode::Address => "30",
            Opcode::Balance => "31",
            Opcode::Origin => "32",
            Opcode::Caller => "33",
            Opcode::Callvalue => "34",
            Opcode::Calldataload => "35",
            Opcode::Calldatasize => "36",
            Opcode::Calldatacopy => "37",
            Opcode::Codesize => "38",
            Opcode::Codecopy => "39",
            Opcode::Gasprice => "3a",
            Opcode::Extcodesize => "3b",
            Opcode::Extcodecopy => "3c",
            Opcode::Returndatasize => "3d",
            Opcode::Returndatacopy => "3e",
            Opcode::Extcodehash => "3f",
            Opcode::Blockhash => "40",
            Opcode::Coinbase => "41",
            Opcode::Timestamp => "42",
            Opcode::Number => "43",
            Opcode::Difficulty => "44",
            Opcode::Prevrandao => "44",
            Opcode::Gaslimit => "45",
            Opcode::Chainid => "46",
            Opcode::Clz => "1e",
            Opcode::Selfbalance => "47",
            Opcode::Basefee => "48",
            Opcode::Blobhash => "49",
            Opcode::Blobbasefee => "4a",
            Opcode::Slotnum => "4b",
            Opcode::Pop => "50",
            Opcode::Mload => "51",
            Opcode::Mstore => "52",
            Opcode::Mstore8 => "53",
            Opcode::Sload => "54",
            Opcode::Sstore => "55",
            Opcode::Jump => "56",
            Opcode::Jumpi => "57",
            Opcode::Pc => "58",
            Opcode::Msize => "59",
            Opcode::Gas => "5a",
            Opcode::Jumpdest => "5b",
            Opcode::Tload => "5c",
            Opcode::Tstore => "5d",
            Opcode::Mcopy => "5e",
            Opcode::Push0 => "5f",
            Opcode::Push1 => "60",
            Opcode::Push2 => "61",
            Opcode::Push3 => "62",
            Opcode::Push4 => "63",
            Opcode::Push5 => "64",
            Opcode::Push6 => "65",
            Opcode::Push7 => "66",
            Opcode::Push8 => "67",
            Opcode::Push9 => "68",
            Opcode::Push10 => "69",
            Opcode::Push11 => "6a",
            Opcode::Push12 => "6b",
            Opcode::Push13 => "6c",
            Opcode::Push14 => "6d",
            Opcode::Push15 => "6e",
            Opcode::Push16 => "6f",
            Opcode::Push17 => "70",
            Opcode::Push18 => "71",
            Opcode::Push19 => "72",
            Opcode::Push20 => "73",
            Opcode::Push21 => "74",
            Opcode::Push22 => "75",
            Opcode::Push23 => "76",
            Opcode::Push24 => "77",
            Opcode::Push25 => "78",
            Opcode::Push26 => "79",
            Opcode::Push27 => "7a",
            Opcode::Push28 => "7b",
            Opcode::Push29 => "7c",
            Opcode::Push30 => "7d",
            Opcode::Push31 => "7e",
            Opcode::Push32 => "7f",
            Opcode::Dup1 => "80",
            Opcode::Dup2 => "81",
            Opcode::Dup3 => "82",
            Opcode::Dup4 => "83",
            Opcode::Dup5 => "84",
            Opcode::Dup6 => "85",
            Opcode::Dup7 => "86",
            Opcode::Dup8 => "87",
            Opcode::Dup9 => "88",
            Opcode::Dup10 => "89",
            Opcode::Dup11 => "8a",
            Opcode::Dup12 => "8b",
            Opcode::Dup13 => "8c",
            Opcode::Dup14 => "8d",
            Opcode::Dup15 => "8e",
            Opcode::Dup16 => "8f",
            Opcode::Swap1 => "90",
            Opcode::Swap2 => "91",
            Opcode::Swap3 => "92",
            Opcode::Swap4 => "93",
            Opcode::Swap5 => "94",
            Opcode::Swap6 => "95",
            Opcode::Swap7 => "96",
            Opcode::Swap8 => "97",
            Opcode::Swap9 => "98",
            Opcode::Swap10 => "99",
            Opcode::Swap11 => "9a",
            Opcode::Swap12 => "9b",
            Opcode::Swap13 => "9c",
            Opcode::Swap14 => "9d",
            Opcode::Swap15 => "9e",
            Opcode::Swap16 => "9f",
            Opcode::Log0 => "a0",
            Opcode::Log1 => "a1",
            Opcode::Log2 => "a2",
            Opcode::Log3 => "a3",
            Opcode::Log4 => "a4",
            Opcode::Dupn => "e6",
            Opcode::Swapn => "e7",
            Opcode::Exchange => "e8",
            Opcode::Create => "f0",
            Opcode::Call => "f1",
            Opcode::Callcode => "f2",
            Opcode::Return => "f3",
            Opcode::Delegatecall => "f4",
            Opcode::Create2 => "f5",
            Opcode::Staticcall => "fa",
            Opcode::Revert => "fd",
            Opcode::Invalid => "fe",
            Opcode::Selfdestruct => "ff",
        };
        opcode_str.to_string()
    }

    /// Returns true if the current opcode is a push opcode that takes a literal value
    pub fn is_value_push(&self) -> bool {
        matches!(
            self,
            Opcode::Push1
                | Opcode::Push2
                | Opcode::Push3
                | Opcode::Push4
                | Opcode::Push5
                | Opcode::Push6
                | Opcode::Push7
                | Opcode::Push8
                | Opcode::Push9
                | Opcode::Push10
                | Opcode::Push11
                | Opcode::Push12
                | Opcode::Push13
                | Opcode::Push14
                | Opcode::Push15
                | Opcode::Push16
                | Opcode::Push17
                | Opcode::Push18
                | Opcode::Push19
                | Opcode::Push20
                | Opcode::Push21
                | Opcode::Push22
                | Opcode::Push23
                | Opcode::Push24
                | Opcode::Push25
                | Opcode::Push26
                | Opcode::Push27
                | Opcode::Push28
                | Opcode::Push29
                | Opcode::Push30
                | Opcode::Push31
                | Opcode::Push32
        )
    }

    /// Prefixes the literal if necessary
    pub fn prefix_push_literal(&self, literal: &str) -> String {
        if self.is_value_push()
            && let Ok(len) = u8::from_str_radix(&self.string(), 16)
            && len >= 96
        {
            let size = (len - 96 + 1) * 2;
            // This case should be caught in the parser
            if literal.len() <= size as usize {
                let zeros_needed = size - literal.len() as u8;
                let zero_prefix = (0..zeros_needed).map(|_| "0").collect::<Vec<&str>>().join("");
                return format!("{zero_prefix}{literal}");
            }
        }

        literal.to_string()
    }

    /// Number of value bytes following a PUSH1..PUSH32 opcode, or `None` for any other opcode
    pub fn push_size(&self) -> Option<usize> {
        if !self.is_value_push() {
            return None;
        }
        u8::from_str_radix(&self.string(), 16).ok().map(|byte| (byte - 0x5f) as usize)
    }

    /// Number of stack-position operands an opcode encodes in its immediate byte (EIP-8024)
    ///
    /// `dupn <n>` and `swapn <n>` take one operand, `exchange <n> <m>` takes two. Every other
    /// opcode returns 0.
    pub fn stack_immediate_operands(&self) -> usize {
        match self {
            Opcode::Dupn | Opcode::Swapn => 1,
            Opcode::Exchange => 2,
            _ => 0,
        }
    }

    /// Returns true if the opcode is followed by a one-byte stack immediate (EIP-8024)
    pub fn has_stack_immediate(&self) -> bool {
        self.stack_immediate_operands() > 0
    }

    /// Encodes the stack operands of a DUPN, SWAPN or EXCHANGE into its immediate byte
    ///
    /// Operands use the same numbering as the EIP: `dupn 17` behaves like a `dup17`, `swapn 17`
    /// like a `swap17`, and `exchange n m` swaps the (n+1)th and (m+1)th stack items.
    ///
    /// Returns `None` if the operands cannot be represented. The encoding never produces a byte
    /// that would be read as JUMPDEST or PUSH1..PUSH32, so emitted code keeps its jump targets.
    pub fn encode_stack_immediate(&self, operands: &[usize]) -> Option<u8> {
        match (self, operands) {
            (Opcode::Dupn | Opcode::Swapn, &[n]) => {
                if !(STACK_IMMEDIATE_MIN_N..=STACK_IMMEDIATE_MAX_N).contains(&n) {
                    return None;
                }
                let x = ((n + 256 - 145) % 256) as u8;
                debug_assert_eq!(decode_single_immediate(x), Some(n));
                Some(x)
            }
            (Opcode::Exchange, &[n, m]) => (0..=u8::MAX).find(|&x| decode_pair_immediate(x) == Some((n, m))),
            _ => None,
        }
    }

    /// Encodes the immediate data that follows the opcode from its resolved operand values
    ///
    /// PUSH1..PUSH32 take one value that must fit the push width; DUPN, SWAPN and EXCHANGE take
    /// stack positions as described in [`Opcode::encode_stack_immediate`]. Returns the immediate
    /// as a hex string, or a description of why the values cannot be encoded.
    pub fn encode_immediate(&self, values: &[U256]) -> Result<String, String> {
        if let Some(size) = self.push_size() {
            let value = values[0];
            if value.byte_len() > size {
                return Err(format!("value {value:#x} does not fit into \"{self:?}\" ({size} bytes)"));
            }
            return Ok(hex::encode(&value.to_be_bytes::<32>()[32 - size..]));
        }

        let positions: Option<Vec<usize>> = values.iter().map(|v| usize::try_from(*v).ok()).collect();
        positions.and_then(|p| self.encode_stack_immediate(&p)).map(|x| format!("{x:02x}")).ok_or_else(|| {
            let got: Vec<String> = values.iter().map(U256::to_string).collect();
            format!("{}, got {}", self.stack_immediate_hint(), got.join(" "))
        })
    }

    /// Describes the valid operand range of a stack-immediate opcode, for error messages
    pub fn stack_immediate_hint(&self) -> String {
        match self {
            Opcode::Dupn | Opcode::Swapn => {
                format!("\"{self:?}\" takes one stack depth between {STACK_IMMEDIATE_MIN_N} and {STACK_IMMEDIATE_MAX_N}")
            }
            Opcode::Exchange => "\"Exchange\" takes two stack positions n and m with 1 <= n < m and n + m <= 30".to_string(),
            _ => String::new(),
        }
    }

    /// Checks if the value overflows the given push opcode
    pub fn push_overflows(&self, literal: &str) -> bool {
        if self.is_value_push()
            && let Ok(len) = u8::from_str_radix(&self.string(), 16)
            && len >= 96
        {
            let size = (len - 96 + 1) * 2;
            return literal.len() > size as usize;
        }

        false
    }

    /// Returns the minimum EVM version required for this opcode
    /// Returns None for opcodes that have been available since the beginning
    pub fn requires_evm_version(&self) -> Option<SupportedEVMVersions> {
        match self {
            // Paris opcodes
            Opcode::Prevrandao => Some(SupportedEVMVersions::Paris),
            // Shanghai opcodes
            Opcode::Push0 => Some(SupportedEVMVersions::Shanghai),
            // Cancun opcodes
            Opcode::Tload | Opcode::Tstore => Some(SupportedEVMVersions::Cancun),
            Opcode::Mcopy => Some(SupportedEVMVersions::Cancun),
            Opcode::Blobhash | Opcode::Blobbasefee => Some(SupportedEVMVersions::Cancun),
            // Osaka opcodes
            Opcode::Clz => Some(SupportedEVMVersions::Osaka),
            // Amsterdam opcodes
            Opcode::Slotnum | Opcode::Dupn | Opcode::Swapn | Opcode::Exchange => Some(SupportedEVMVersions::Amsterdam),
            // All other opcodes are available since before Paris
            _ => None,
        }
    }
}

/// Smallest stack depth reachable by DUPN and SWAPN
pub const STACK_IMMEDIATE_MIN_N: usize = 17;
/// Largest stack depth reachable by DUPN and SWAPN
pub const STACK_IMMEDIATE_MAX_N: usize = 235;

/// Decodes a DUPN/SWAPN immediate byte as defined by EIP-8024
fn decode_single_immediate(x: u8) -> Option<usize> {
    let x = x as usize;
    if x <= 0x5a || x >= 0x80 { Some((x + 145) % 256) } else { None }
}

/// Decodes an EXCHANGE immediate byte into its `(n, m)` pair as defined by EIP-8024
fn decode_pair_immediate(x: u8) -> Option<(usize, usize)> {
    let x = x as usize;
    if x > 0x51 && x < 0x80 {
        return None;
    }
    let k = x ^ 0x8f;
    let (q, r) = (k / 16, k % 16);
    if q < r { Some((q + 1, r + 1)) } else { Some((r + 1, 29 - q)) }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let opcode_str = self.string();
        write!(f, "{opcode_str}")
    }
}

impl From<Opcode> for String {
    fn from(o: Opcode) -> Self {
        o.string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test if the opcode array and map are in sync
    #[test]
    fn test_opcode_array_equal() {
        for opcode in OPCODES {
            assert!(OPCODES_MAP.contains_key(opcode), "{opcode}");
        }
        for opcode in OPCODES_MAP.keys() {
            assert!(OPCODES.contains(opcode), "{opcode}");
        }
    }

    /// Byte values that would turn the immediate into a JUMPDEST or PUSH1..PUSH32
    fn is_forbidden_immediate(x: u8) -> bool {
        (0x5b..=0x7f).contains(&x)
    }

    #[test]
    fn test_dupn_swapn_encoding_round_trips() {
        for opcode in [Opcode::Dupn, Opcode::Swapn] {
            for n in STACK_IMMEDIATE_MIN_N..=STACK_IMMEDIATE_MAX_N {
                let x = opcode.encode_stack_immediate(&[n]).unwrap_or_else(|| panic!("{opcode:?} {n} must be encodable"));
                assert!(!is_forbidden_immediate(x), "{opcode:?} {n} encoded to forbidden byte {x:#04x}");
                assert_eq!(decode_single_immediate(x), Some(n));
            }
            assert_eq!(opcode.encode_stack_immediate(&[0]), None);
            assert_eq!(opcode.encode_stack_immediate(&[16]), None);
            assert_eq!(opcode.encode_stack_immediate(&[236]), None);
            assert_eq!(opcode.encode_stack_immediate(&[17, 18]), None);
        }
    }

    #[test]
    fn test_dupn_swapn_known_vectors() {
        // Reference values from EIP-8024
        assert_eq!(Opcode::Dupn.encode_stack_immediate(&[17]), Some(0x80));
        assert_eq!(Opcode::Dupn.encode_stack_immediate(&[144]), Some(0xff));
        assert_eq!(Opcode::Dupn.encode_stack_immediate(&[145]), Some(0x00));
        assert_eq!(Opcode::Swapn.encode_stack_immediate(&[235]), Some(0x5a));
    }

    #[test]
    fn test_exchange_encoding_covers_all_valid_pairs() {
        let mut count = 0;
        for n in 1..30 {
            for m in (n + 1)..=(30 - n) {
                let x = Opcode::Exchange.encode_stack_immediate(&[n, m]).unwrap_or_else(|| panic!("exchange {n} {m} must be encodable"));
                assert!(!is_forbidden_immediate(x), "exchange {n} {m} encoded to forbidden byte {x:#04x}");
                assert_eq!(decode_pair_immediate(x), Some((n, m)));
                count += 1;
            }
        }
        // Every non-forbidden byte decodes to exactly one distinct pair
        let valid_bytes = (0..=u8::MAX).filter(|&x| decode_pair_immediate(x).is_some()).count();
        assert_eq!(count, valid_bytes);

        // Reference value from EIP-8024: 0x8e swaps the second and third stack items
        assert_eq!(Opcode::Exchange.encode_stack_immediate(&[1, 2]), Some(0x8e));

        assert_eq!(Opcode::Exchange.encode_stack_immediate(&[2, 1]), None);
        assert_eq!(Opcode::Exchange.encode_stack_immediate(&[1, 1]), None);
        assert_eq!(Opcode::Exchange.encode_stack_immediate(&[0, 5]), None);
        assert_eq!(Opcode::Exchange.encode_stack_immediate(&[14, 17]), None);
        assert_eq!(Opcode::Exchange.encode_stack_immediate(&[3]), None);
    }

    #[test]
    fn test_stack_immediate_operands() {
        assert_eq!(Opcode::Dupn.stack_immediate_operands(), 1);
        assert_eq!(Opcode::Swapn.stack_immediate_operands(), 1);
        assert_eq!(Opcode::Exchange.stack_immediate_operands(), 2);
        assert!(!Opcode::Dup16.has_stack_immediate());
        assert!(!Opcode::Slotnum.has_stack_immediate());
        assert_eq!(Opcode::Dup16.encode_stack_immediate(&[17]), None);
    }

    /// Validate that opcodes are ordered alphabetically with same prefix and then decreasing length
    #[test]
    fn test_opcode_order() {
        let mut sorted_opcodes = OPCODES.to_vec();
        sorted_opcodes.sort_by(|a, b| {
            // Find the common prefix length
            let common_len = a.chars().zip(b.chars()).take_while(|(c1, c2)| c1 == c2).count();

            // If one string is a prefix of another, group them together and sort by length (descending)
            if common_len == a.len().min(b.len()) {
                b.len().cmp(&a.len()) // Longer string first
            } else {
                a.cmp(b) // Regular alphabetical sort
            }
        });
        assert_eq!(OPCODES.to_vec(), sorted_opcodes, "Opcodes are not ordered correctly");
    }
}
