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
tests, and are formatted as follows: `#[flag_a(inputs...), flag_b(inputs...)]`

Available decorators include:
* `calldata` - Set the calldata for the transaction environment. Accepts a single string of calldata bytes.
* `value` - Set the callvalue for the transaction environment. Accepts a single literal.

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