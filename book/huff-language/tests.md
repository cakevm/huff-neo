# Tests

The compiler includes a simple, stripped-down testing framework to assist in creating assertions
as well as gas profiling macros and functions. huff-rs’ test suite is intentionally lacking in features,
and with that, this addition is not meant to replace developers’ dependency on `foundry-huff`. Ideally,
for contracts that will be in production, Huff developers will utilize both foundry and Huff tests.
If you are one of many who uses Huff as a tool for learning, Huff tests can also be a lighter weight
experience when testing your contract’s logic.

Tests can be run via the CLI's `test` subcommand. For more information, see the [CLI Resources](../cli.md).

## Decorators
The transaction environment for each test can be modified with a decorator. Decorators sit directly above
tests, and are formatted as follows: `#[flag_a(inputs...), flag_b(inputs...)]`. Flags without a comma
between them still parse for compatibility with older sources, but write the comma.

A test can have several decorator blocks. They are merged in source order, so these two tests are
configured the same way, and a trailing comma is allowed:

```javascript
#[calldata("0x01"), value(0x01)]
#define test SINGLE_BLOCK() = { stop }

#[calldata("0x01"),]
#[value(0x01)]
#define test STACKED_BLOCKS() = { stop }
```

Decorators only apply to tests. A decorator above a `#define macro` or `#define fn` is a compile error.

Available decorators include:
* `calldata(bytes...)` - Set the calldata of the test transaction. The arguments are joined into one byte sequence
  (see below), e.g. `calldata(__FUNC_SIG(transfer), caller, 100)`.
* `value(word)` - Set the callvalue of the test transaction, e.g. `value(0x01)` or `value([AMOUNT])`.
* `store(slot, value, at = address)` - Set a storage slot before the test runs. `at` is optional and defaults to the
  test itself. `store` can be repeated, but setting the same slot of the same address twice is an error.
* `deal(address, wei)` - Set the balance of an account before the test runs. The sender of the test has an unlimited
  balance unless it is dealt one.
* `etch(address, bytes...)` - Put code at an address before the test runs, e.g. `etch(0x1000, "0x5f5ff3")`. The
  arguments are joined into one byte sequence like those of `calldata`. The test itself and its sender cannot be etched.
* `nonce(address, n)` - Set the nonce of an account before the test runs.

`store`, `deal`, `etch` and `nonce` can be repeated, but setting the same slot, balance, code or nonce of the same
address twice is an error.

The test transaction and its block can be changed with these decorators, each taking one value:

| Decorator           | Sets                                               | Example                 |
|---------------------|----------------------------------------------------|-------------------------|
| `caller(address)`   | the sender of the test, both `caller` and `origin` | `caller(0xbeef)`        |
| `gas_limit(n)`      | the gas limit of the test transaction              | `gas_limit(100000)`     |
| `gas_price(n)`      | `gasprice`                                         | `gas_price(10)`         |
| `block_number(n)`   | `number`                                           | `block_number(100)`     |
| `timestamp(n)`      | `timestamp`                                        | `timestamp(1700000000)` |
| `basefee(n)`        | `basefee`                                          | `basefee(7)`            |
| `blob_basefee(n)`   | `blobbasefee`                                      | `blob_basefee(1)`       |
| `coinbase(address)` | `coinbase`                                         | `coinbase(0xc0ffee)`    |
| `chain_id(n)`       | `chainid`                                          | `chain_id(1)`           |
| `prevrandao(word)`  | `prevrandao`                                       | `prevrandao(0x2a)`      |
| `slot_number(n)`    | `slotnum` (Amsterdam)                              | `slot_number(100)`      |

These values are fixed for the whole test, as they are for a real transaction. A value that is not set keeps the
default of the test runner, which `hnc test` takes from the Foundry configuration and its CLI flags.

`calldata`, `value` and the decorators above may only be given once per test. An unknown decorator flag, or a flag
with the wrong arguments, fails `hnc test` and points at the flag in the source.

### Decorator values
A value that stands for a 32-byte word (`value`, the slot, value and address of `store`, and the values of the
transaction and block decorators) can be written as:
* a hex literal (`0xbeef`), a decimal literal (`100`), or `true` / `false`
* a constant (`[OWNER_SLOT]`), including `FREE_STORAGE_POINTER()` constants
* a builtin call such as `__FUNC_SIG(transfer)` or `__EVENT_HASH(Transfer)`
* `this` for the address of the test, or `caller` for the sender of the test transaction, as set by `caller(..)`

A word is the same value Huff would push for it, so `store([OWNER_SLOT], 0xbeef)` sets the slot that
`[OWNER_SLOT] sload` reads. Shorter values are left-padded with zeros. This also applies to addresses:
`at = 0x1000` is the address `0x0000000000000000000000000000000000001000`. A value that does not fit in 20 bytes is not an address and fails the test.

The arguments of `calldata` are joined into one byte sequence:
* `__FUNC_SIG(..)` and `__ERROR(..)` add their 4-byte selector
* a hex literal adds its bytes as written, so `0x0001` adds two bytes
* a string such as `"0x0102"` adds its hex bytes
* every other value adds a 32-byte word, so `100` adds 32 bytes

```javascript
#define function transfer(address,uint256) nonpayable returns ()
#define constant OWNER_SLOT = FREE_STORAGE_POINTER()

#[calldata(__FUNC_SIG(transfer), caller, 100)]
#[store([OWNER_SLOT], 0xbeef), store(0x01, 0x02, at = 0x1000)]
#define test TRANSFER() = {
    [OWNER_SLOT] sload  // [0xbeef]
    ...
}
```

### Expectations
A test passes when it ends the way its decorators expect. Without an expectation, a test must return: `stop`,
`return`, or running off the end of its code. A test that reverts or halts fails.

| Decorator                         | The test passes when it                                           |
|-----------------------------------|-------------------------------------------------------------------|
| `expect_revert`                   | reverts, with any data                                            |
| `expect_revert(bytes...)`         | reverts with exactly this data                                    |
| `expect_partial_revert(bytes...)` | reverts with data that starts with this                           |
| `expect_halt`                     | halts exceptionally, e.g. on `invalid`, out of gas or a bad jump  |
| `expect_halt("Reason")`           | halts for this reason                                             |
| `expect_return(bytes...)`         | returns exactly this data; `expect_return` alone means no data    |
| `expect_gas(min = n, max = m)`    | uses at least `min` and at most `max` gas; either can be left out |

The state after the test can be checked too. These expectations can be repeated, once per slot or address:

| Decorator                                   | The test passes when, after the test                        |
|---------------------------------------------|-------------------------------------------------------------|
| `expect_storage(slot, value, at = address)` | the slot holds this value; `at` defaults to the test itself |
| `expect_storage(slot, min = n, max = m)`    | the slot holds a value from `min` to `max`                  |
| `expect_balance(address, wei)`              | the account holds exactly this balance                      |
| `expect_balance(address, min = n, max = m)` | the balance is from `min` to `max`                          |

Bounds are inclusive, like those of `expect_gas`, and either one can be left out: `min = 1` means at least 1.
Storage values are compared as unsigned 256-bit numbers. `at` also works together with `min` and `max`.

```javascript
#define constant COUNTER_SLOT = FREE_STORAGE_POINTER()
#define macro INCREMENT() = { [COUNTER_SLOT] sload 0x01 add [COUNTER_SLOT] sstore }
#define macro MAIN() = { INCREMENT() stop }

#[value(5), store([COUNTER_SLOT], 0x01)]
#[expect_storage([COUNTER_SLOT], 0x02), expect_balance(this, 5)]
#define test INCREMENT_ADDS_ONE() = { INCREMENT() }
```

A reverted or halted test keeps no state, so `expect_storage` and `expect_balance` cannot be combined with
`expect_revert`, `expect_partial_revert` or `expect_halt`. When a test reverts without expecting it, only the
unexpected revert is reported.

The arguments of `expect_revert`, `expect_partial_revert` and `expect_return` are byte sequences, built like the
arguments of `calldata`:

```javascript
#define error InsufficientBalance(uint256)

#[calldata(__FUNC_SIG(withdraw), 1000)]
#[expect_revert(__ERROR(InsufficientBalance), 100)]  // selector + the uint256 argument
#define test WITHDRAW_TOO_MUCH() = { MAIN() }

#[expect_halt("InvalidJump")]
#define test BAD_JUMP() = { 0x00 jump }
```

A test can expect only one way to end, so `expect_revert`, `expect_partial_revert`, `expect_halt` and
`expect_return` exclude each other. A revert never matches `expect_halt`, and a halt never matches `expect_revert`.

Halt reasons are the names of the EVM's halt reasons, such as `OutOfGas`, `InvalidJump`, `InvalidFEOpcode` (the
`invalid` opcode), `OpcodeNotFound`, `StackUnderflow`, `StackOverflow` and `NotActivated`. `OutOfGas` matches every
kind of out-of-gas halt.

The gas checked by `expect_gas` is the gas the test reports, which leaves out the fixed cost of the test transaction.
An exceptional halt uses up all the gas of the transaction.

A failing test lists every expectation it did not meet:

```javascript
#[expect_revert(__ERROR(Unauthorized)), expect_gas(max = 1)]
#define test FAILS_TWICE() = { __ERROR(InsufficientBalance) 0x00 mstore 0x04 0x00 revert }
```

```text
[FAIL] FAILS_TWICE     - Gas used: 16
├─ expect_revert: expected revert data 0x82b42900 (Unauthorized), got 0x92665351 (InsufficientBalance)
├─ expect_gas: used 16 gas, more than max 1
```

With `--format json`, each result has a `passed` flag, an `outcome` (`return`, `revert` or `halt`, with its data or
reason) and the list of `failures`.

`hnc test` does not support Foundry cheatcodes yet. A test that calls the cheatcode address
`0x7109709ECfa91a80626fF3989D68f67F5b1DD12D` fails instead of silently doing nothing.

### Labels defined in MAIN
A macro often jumps to a label that `MAIN` defines, such as a shared error exit. A test that invokes the macro
directly does not compile, because the label does not exist in the test: `Unmatched Jump Labels: "unauthorized"`.

Define the label in the test instead. A macro sees the labels of the code that invokes it, so the jump lands in
the test, which can end the way the label in `MAIN` would and check it with an expectation:

```javascript
#define error Unauthorized()
#define constant OWNER_SLOT = FREE_STORAGE_POINTER()

// Jumps to MAIN's `unauthorized` label unless the caller is the owner
#define macro ONLY_OWNER() = takes (0) returns (0) {
    [OWNER_SLOT] sload caller eq iszero unauthorized jumpi
}

#define macro MAIN() = takes (0) returns (0) {
    ONLY_OWNER()
    stop

    unauthorized:
        __ERROR(Unauthorized) 0x00 mstore 0x04 0x00 revert
}

#[store([OWNER_SLOT], 0xbeef), caller(0xdead), expect_revert(__ERROR(Unauthorized))]
#define test REJECTS_STRANGER() = {
    ONLY_OWNER()
    stop

    unauthorized:
        __ERROR(Unauthorized) 0x00 mstore 0x04 0x00 revert
}
```

When several tests need the same labels, define them once in a macro and invoke it after the test code. The
other macros the test invokes see its labels too:

```javascript
#define macro TERMINALS() = {
    unauthorized:
        __ERROR(Unauthorized) 0x00 mstore 0x04 0x00 revert
}

#[store([OWNER_SLOT], 0xbeef), caller(0xdead), expect_revert(__ERROR(Unauthorized))]
#define test REJECTS_STRANGER() = { ONLY_OWNER() stop TERMINALS() }

#[store([OWNER_SLOT], 0xbeef), caller(0xbeef)]
#define test ALLOWS_OWNER() = { ONLY_OWNER() stop TERMINALS() }
```

## Example
```javascript
#include "huffmate/utils/Errors.huff"

#define macro ADD_TWO() = takes (2) returns (1) {
    // Input Stack:  [a, b]
    add           // [a + b]
    // Return Stack: [a + b]
}

#[calldata("0x0000000000000000000000000000000000000000000000000000000000000001"), value(0x01)]
#define test MY_TEST() = {
    0x00 calldataload   // [0x01]
    callvalue           // [0x01, 0x01]
    eq ASSERT()
}
```