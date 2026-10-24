# Tests

Huff tests are macros defined with `#define test`. Each test is compiled on its own, deployed, and called in one
transaction. Decorators above a test set up that transaction and the state it starts from, and say how the test
must end and what state it must leave behind.

```javascript
#define macro ADD() = takes (2) returns (1) { add }

#[expect_return(3)]
#define test ADDS() = {
    0x01 0x02 ADD()        // [3]
    0x00 mstore
    0x20 0x00 return
}
```

Run the tests of a file with `hnc test`. For more information, see the [CLI Resources](../cli.md).

`hnc test` runs each test on Foundry's EVM executor, the same engine as `forge test`. The test is deployed at
Foundry's test address `0x7FA9385bE102ac3EAc297483Dd6233D62b3e1496` and called from Foundry's default sender
`0x1804c8AB1F12E6bbf3894d4083f33e07309d1f38`, unless `caller(..)`, `--sender` or `--target-address` change them.

A test can call [Foundry cheatcodes](https://getfoundry.sh/reference/cheatcodes/overview) at
`0x7109709ECfa91a80626fF3989D68f67F5b1DD12D`, for example to change the block in the middle of a test. A failed
`vm.assert*` fails the test; run with `-vv` to see its message. For tests written in Solidity that call a deployed
Huff contract, use Foundry with [foundry-huff-neo](https://github.com/cakevm/foundry-huff-neo).

## Decorators
Decorators sit directly above a test and hold one or more flags: `#[flag_a(inputs...), flag_b(inputs...)]`. A test
can have several decorator blocks. They are merged in source order, so these two tests are configured the same way,
and a trailing comma is allowed:

```javascript
#[calldata("0x01"), value(0x01)]
#define test SINGLE_BLOCK() = { stop }

#[calldata("0x01"),]
#[value(0x01)]
#define test STACKED_BLOCKS() = { stop }
```

A suggested style is one block per concern, in the order transaction, state, expectations. Flags without a comma
between them still parse for compatibility with older sources, but write the comma.

Decorators only apply to tests. A decorator above a `#define macro` or `#define fn` is a compile error. An unknown
flag, a flag with the wrong arguments, or a flag given twice where only one is allowed fails `hnc test` and points at
the flag in the source.

### Transaction and block
These decorators set the test transaction and its block. Each may be given once per test.

| Decorator            | Sets                                               | Example                          |
|----------------------|----------------------------------------------------|----------------------------------|
| `calldata(bytes...)` | the calldata of the test transaction               | `calldata(__FUNC_SIG(transfer))` |
| `value(n)`           | `callvalue`                                        | `value(100)`                     |
| `caller(address)`    | the sender of the test, both `caller` and `origin` | `caller(0xbeef)`                 |
| `gas_limit(n)`       | the gas limit of the test transaction              | `gas_limit(100000)`              |
| `gas_price(n)`       | `gasprice`                                         | `gas_price(10)`                  |
| `block_number(n)`    | `number`                                           | `block_number(100)`              |
| `timestamp(n)`       | `timestamp`                                        | `timestamp(1700000000)`          |
| `basefee(n)`         | `basefee`                                          | `basefee(7)`                     |
| `blob_basefee(n)`    | `blobbasefee`                                      | `blob_basefee(1)`                |
| `coinbase(address)`  | `coinbase`                                         | `coinbase(0xc0ffee)`             |
| `chain_id(n)`        | `chainid`                                          | `chain_id(1)`                    |
| `prevrandao(word)`   | `prevrandao`                                       | `prevrandao(0x2a)`               |
| `slot_number(n)`     | `slotnum` (Amsterdam)                              | `slot_number(100)`               |

These values are fixed for the whole test, as they are for a real transaction. A value that is not set keeps the
default of the test runner, which `hnc test` takes from the Foundry configuration and its CLI flags.

### State before the test
These decorators set up accounts before the test runs. Each can be repeated, but setting the same slot, balance,
code or nonce of the same address twice is an error that points at both flags.

| Decorator                          | Sets                                                              |
|------------------------------------|-------------------------------------------------------------------|
| `store(slot, value, at = address)` | a storage slot; `at` defaults to the test itself                  |
| `deal(address, wei)`               | the balance of an account                                         |
| `etch(address, bytes...)`          | the code at an address, e.g. `etch(0x1000, "0x5f5ff3")`           |
| `nonce(address, n)`                | the nonce of an account                                           |

The sender of the test has an unlimited balance unless it is dealt one. The test itself and its sender cannot be
etched.

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

A test can expect only one way to end, so `expect_revert`, `expect_partial_revert`, `expect_halt` and
`expect_return` exclude each other. A revert never matches `expect_halt`, and a halt never matches `expect_revert`.

```javascript
#define error InsufficientBalance(uint256)

#[expect_revert(__ERROR(InsufficientBalance), 100)]  // selector + the uint256 argument
#define test REVERTS() = {
    __ERROR(InsufficientBalance) 0x00 mstore
    0x64 0x04 mstore     // 100
    0x24 0x00 revert
}

#[expect_halt("InvalidJump")]
#define test BAD_JUMP() = { 0x00 jump }
```

Halt reasons are the names of the EVM's halt reasons, such as `OutOfGas`, `InvalidJump`, `InvalidFEOpcode` (the
`invalid` opcode), `OpcodeNotFound`, `StackUnderflow`, `StackOverflow` and `NotActivated`. `OutOfGas` matches every
kind of out-of-gas halt.

The gas checked by `expect_gas` is the gas the test reports, which leaves out the fixed cost of the test transaction.
An exceptional halt uses up all the gas of the transaction.

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

#[value(5), store([COUNTER_SLOT], 0x01)]
#[expect_storage([COUNTER_SLOT], 0x02), expect_balance(this, 5)]
#define test INCREMENT_ADDS_ONE() = { INCREMENT() }
```

A reverted or halted test keeps no state, so `expect_storage` and `expect_balance` cannot be combined with
`expect_revert`, `expect_partial_revert` or `expect_halt`. When a test reverts without expecting it, only the
unexpected revert is reported.

### Decorator values
A value that stands for a 32-byte word, such as a slot, an amount, an address or a bound, can be written as:
* a hex literal (`0xbeef`), a decimal literal (`100`), or `true` / `false`
* a constant (`[OWNER_SLOT]`), including `FREE_STORAGE_POINTER()` constants
* a builtin call such as `__FUNC_SIG(transfer)` or `__EVENT_HASH(Transfer)`
* `this` for the address of the test, or `caller` for the sender of the test transaction, as set by `caller(..)`

A word is the same value Huff would push for it, so `store([OWNER_SLOT], 0xbeef)` sets the slot that
`[OWNER_SLOT] sload` reads. Shorter values are left-padded with zeros. This also applies to addresses:
`at = 0x1000` is the address `0x0000000000000000000000000000000000001000`. A value that does not fit in 20 bytes is
not an address and fails the test.

The arguments of `calldata`, `etch`, `expect_revert`, `expect_partial_revert` and `expect_return` are joined into one
byte sequence:
* `__FUNC_SIG(..)` and `__ERROR(..)` add their 4-byte selector
* a hex literal adds its bytes as written, so `0x0001` adds two bytes
* a string such as `"0x0102"` adds its hex bytes
* every other value adds a 32-byte word, so `100` adds 32 bytes

So `calldata(__FUNC_SIG(transfer), caller, 100)` is the ABI encoding of `transfer(caller, 100)`.

## Failures and output
A failing test lists every expectation it did not meet:

```javascript
#define error Unauthorized()
#define error InsufficientBalance(uint256)

#[expect_revert(__ERROR(Unauthorized)), expect_gas(max = 1)]
#define test FAILS_TWICE() = { __ERROR(InsufficientBalance) 0x00 mstore 0x04 0x00 revert }
```

```text
[FAIL] FAILS_TWICE     - Gas used: 16
├─ expect_revert: expected revert data 0x82b42900 (Unauthorized), got 0x92665351 (InsufficientBalance)
├─ expect_gas: used 16 gas, more than max 1
```

With `--format json`, each result has a `passed` flag, an `outcome` (`return`, `revert` or `halt`, with its data or
reason) and the list of `failures`. `hnc test` exits with status 1 when a test fails.

Run with `-vv` to see console logs, `-vvv` to see the execution trace of failing tests, `-vvvv` for the traces of all
tests, and `-vvvvv` to add storage changes to the traces, as in `forge test`. See the [CLI Resources](../cli.md).

## Labels defined in MAIN
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
A counter that only its owner can increment, tested through `MAIN`:

```javascript
#define function increment() nonpayable returns ()
#define function count() view returns (uint256)

#define error Unauthorized()

#define constant OWNER_SLOT = FREE_STORAGE_POINTER()
#define constant COUNT_SLOT = FREE_STORAGE_POINTER()

// Jumps to MAIN's `unauthorized` label unless the caller is the owner
#define macro ONLY_OWNER() = takes (0) returns (0) {
    [OWNER_SLOT] sload caller eq iszero unauthorized jumpi
}

#define macro INCREMENT() = takes (0) returns (0) {
    ONLY_OWNER()
    [COUNT_SLOT] sload 0x01 add [COUNT_SLOT] sstore
    stop
}

#define macro COUNT() = takes (0) returns (0) {
    [COUNT_SLOT] sload 0x00 mstore
    0x20 0x00 return
}

#define macro MAIN() = takes (0) returns (0) {
    0x00 calldataload 0xe0 shr
    dup1 __FUNC_SIG(increment) eq increment jumpi
    dup1 __FUNC_SIG(count) eq count jumpi
    0x00 0x00 revert

    increment:
        INCREMENT()
    count:
        COUNT()
    unauthorized:
        __ERROR(Unauthorized) 0x00 mstore 0x04 0x00 revert
}

#[calldata(__FUNC_SIG(count)), store([COUNT_SLOT], 7)]
#[expect_return(7)]
#define test RETURNS_THE_COUNT() = { MAIN() }

#[calldata(__FUNC_SIG(increment)), caller(0xbeef)]
#[store([OWNER_SLOT], 0xbeef), store([COUNT_SLOT], 1)]
#[expect_storage([COUNT_SLOT], 2), expect_gas(max = 30000)]
#define test OWNER_INCREMENTS() = { MAIN() }

#[calldata(__FUNC_SIG(increment)), caller(0xdead), store([OWNER_SLOT], 0xbeef)]
#[expect_revert(__ERROR(Unauthorized))]
#define test STRANGER_CANNOT_INCREMENT() = { MAIN() }
```
