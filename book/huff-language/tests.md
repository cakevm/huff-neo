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

`calldata` and `value` may only be given once per test. An unknown decorator flag, or a flag with the wrong
arguments, fails `hnc test` and points at the flag in the source.

### Decorator values
A value that stands for a 32-byte word (`value`, and the slot, value and address of `store`) can be written as:
* a hex literal (`0xbeef`), a decimal literal (`100`), or `true` / `false`
* a constant (`[OWNER_SLOT]`), including `FREE_STORAGE_POINTER()` constants
* a builtin call such as `__FUNC_SIG(transfer)` or `__EVENT_HASH(Transfer)`
* `this` for the address of the test, or `caller` for the sender of the test transaction

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