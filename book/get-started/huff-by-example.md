# Huff by Example

## Introduction

Huff by Example is an effort to provide a thorough explanation of each
feature of the Huff language, along with code-snippet examples detailing how,
when, where, and why to use each one. The snippets here are heavily 
commentated, but this section does assume some prior experience working 
with the EVM.

If you are new to low-level EVM programming, please read the
[Tutorials](../tutorial/overview.md) section of the docs before diving into
Huff development. If you run into any issues, please feel free to come ask
the community questions on [Telegram](https://t.me/huff_neo).

## Defining your Interface

While defining an interface is not a necessary step, `functions` and `events`
can be defined in Huff contracts for two purposes: To be used as arguments 
for the `__FUNC_SIG` and `__EVENT_HASH` builtins, and to generate a Solidity 
Interface / Contract ABI.

Functions can be of type `view`, `pure`, `payable` or `nonpayable`, and
function interfaces should only be defined for externally facing functions.

Events can contain `indexed` and non-indexed values.

#### Example

```javascript
#define function testFunction(uint256, bytes32) view returns (bytes memory)

#define event TestEvent(address indexed, uint256)
```

## Constants

Constants in Huff contracts are not included in the contract's storage; Instead,
they are able to be called within the contract at compile time. Constants
can either be bytes (32 max) or a `FREE_STORAGE_POINTER`. A `FREE_STORAGE_POINTER`
constant will always represent an unused storage slot in the contract.

In order to push a constant to the stack, use bracket notation: `[CONSTANT]`

#### Example

**Constant Declaration**
```javascript
#define constant NUM = 0x420
#define constant HELLO_WORLD = 0x48656c6c6f2c20576f726c6421
#define constant FREE_STORAGE = FREE_STORAGE_POINTER()
```

**Constant Usage**
(without loss of generality, let's say the constant `NUM` holds 0x420 from the above example)
```plaintext
                    // [] - an empty stack
[NUM]               // [0x420] - the constant's value is pushed to the stack
```

## Custom Errors

Custom errors can be defined and used by the `__ERROR` builtin to push the left-padded 4 byte
error selector to the stack.

#### Example
```javascript
// Define our custom error
#define error PanicError(uint256)
#define error Error(string)

#define macro PANIC() = takes (1) returns (0) {
    // Input stack:          [panic_code]
    __ERROR(PanicError)   // [panic_error_selector, panic_code]
    0x00 mstore           // [panic_code]
    0x04 mstore           // []
    0x24 0x00 revert
}

#define macro REQUIRE() = takes (3) returns (0) {
    // Input stack:          [condition, message_length, message]
    continue jumpi        // [message_length, message]

    __ERROR(Error)        // [error_selector, message_length, message]
    0x00 mstore           // [message_length, message]
    0x20 0x04 mstore      // [message_length, message]
    0x24 mstore           // [message]
    0x44 mstore           // []

    0x64 0x00 revert

    continue:
        pop               // []
}
```

## Jump Labels

Jump Labels are a simple abstraction included into the language to make defining
and referring to `JUMPDEST`s more simple for the developer.

#### Example

```javascript
#define macro MAIN() = takes (0) returns (0) {
    // Store "Hello, World!" in memory
    0x48656c6c6f2c20576f726c6421
    0x00 mstore // ["Hello, World!"]

    // Jump to success label, skipping the revert statement
    success     // [success_label_pc, "Hello, World!"]
    jump        // ["Hello, World!"]

    // Revert if this point is reached
    0x00 0x00 revert

    // Labels are defined within macros or functions, and are designated
    // by a word followed by a colon. Note that while it may appear as if
    // labels are scoped code blocks due to the indentation, they are simply
    // destinations to jump to in the bytecode. If operations exist below a label,
    // they will be executed unless the program counter is altered or execution is
    // halted by a `revert`, `return`, `stop`, or `selfdestruct` opcode.
    success:
        0x00 mstore
        0x20 0x00 return
}
```

## Macros and Functions

Huff offers two ways to group together your bytecode: Macros and Functions. It is
important to understand the difference between the two, and when to use one
over the other.

Both are defined similarly, taking optional arguments as well as being followed
by the `takes` and `returns` keywords. These designate the amount of stack
inputs the macro/function takes in as well as the amount of stack elements the
macro/function outputs. The `takes` and `returns` keywords are optional - if they are
not present, the value will default to `0`.

```javascript
#define <macro|fn> TEST(err) = takes (1) returns (3) {
    // ...
}
```

### Macros

Most of the time, Huff developers should opt to use macros. Each time a macro is invoked,
the code within it is placed at the point of invocation. This is efficient in
terms of runtime gas cost due to not having to jump to and from the macro's code, 
but it can quickly increase the size of the contract's bytecode if it is used commonly
throughout.

#### Constructor and Main

`MAIN` and `CONSTRUCTOR` are two important macros that serve special purposes. When
your contract is called, the `MAIN` macro will be the fallback, and it is commonly where
a Huff contract's control flow begins. The `CONSTRUCTOR` macro, while not required,
can be used to initialize the contract upon deployment. Inputs to the `CONSTRUCTOR` macro
are provided at compile time.

By default, the `CONSTRUCTOR` will add some bootstrap code that returns the compiled MAIN macro
as the contract's runtime bytecode. If the constructor contains a `RETURN` opcode, the compiler
will not include this bootstrap, and it will instead instantiate the contract with the code returned
by the constructor.

#### Macro Arguments

Macros can accept arguments to be "called" inside the macro or passed as a reference. Macro arguments may be one of: label, opcode, literal, or a constant. Since macros are inlined at compile-time, the arguments are not evaluated at runtime and are instead inlined as well.

#### Example

```javascript
// Define the contract's interface
#define function addWord(uint256) pure returns (uint256)

// Get a free storage slot to store the owner
#define constant OWNER = FREE_STORAGE_POINTER()

// Define the event we wish to emit
#define event WordAdded(uint256 initial, uint256 increment)

// Macro to emit an event that a word has been added
#define macro emitWordAdded(increment) = takes (1) returns (0) {
    // input stack: [initial]
    <increment>              // [increment, initial]
    __EVENT_HASH(WordAdded)  // [sig, increment, initial]
    0x00 0x00                // [mem_start, mem_end, sig, increment, initial]
    log3                     // []
}

// Only owner function modifier
#define macro ONLY_OWNER() = takes (0) returns (0) {
    caller                   // [msg.sender]
    [OWNER] sload            // [owner, msg.sender]
    eq                       // [owner == msg.sender]
    is_owner jumpi           // []

    // Revert if the sender is not the owner
    0x00 0x00 revert

    is_owner:
}

// Add a word (32 bytes) to a uint 
#define macro ADD_WORD() = takes (1) returns (1) {
    // Input Stack:          // [input_num]

    // Enforce that the caller is the owner. The code of the
    // `ONLY_OWNER` macro will be pasted at this invocation. 
    ONLY_OWNER()

    // Call our helper macro that emits an event when a word is added
    // Here we pass a literal that represents how much we increment the word by.
    // NOTE: We need to duplicate the input number on our stack since
    //       emitWordAdded takes 1 stack item and returns 0
    dup1                     // [input_num, input_num]
    emitWordAdded(0x20)      // [input_num]

    // NOTE: 0x20 is automatically pushed to the stack, it is assumed to be a 
    // literal by the compiler.
    0x20                     // [0x20, input_num]
    add                      // [0x20 + input_num]

    // Return stack:            [0x20 + input_num]
}

#define macro MAIN() = takes (0) returns (0) {
    // Get the function signature from the calldata
    0x00 calldataload        // [calldata @ 0x00]
    0xE0 shr                 // [func_sig (calldata @ 0x00 >> 0xE0)]

    // Check if the function signature in the calldata is
    // a match to our `addWord` function definition.
    // More about the `__FUNC_SIG` builtin in the `Builtin Functions`
    // section.
    __FUNC_SIG(addWord)      // [func_sig(addWord), func_sig]
    eq                       // [func_sig(addWord) == func_sig]
    add_word jumpi           // []

    // Revert if no function signature matched
    0x00 0x00 revert

    // Create a jump label
    add_word:
        // Call the `ADD_WORD` macro with the first calldata
        // input, store the result in memory, and return it.
        0x04 calldataload    // [input_num]
        ADD_WORD()           // [result]
        0x00 mstore          // []
        0x20 0x00 return
}
```

### Functions

Functions look extremely similar to macros, but behave somewhat differently.
Instead of the code being inserted at each invocation, the compiler moves
the code to the end of the runtime bytecode, and a jump to and from that
code is inserted at the points of invocation instead. This can be a useful 
abstraction when a certain set of operations is used repeatedly throughout 
your contract, and it is essentially a trade-off of decreasing contract size 
for a small extra runtime gas cost (`22 + n_inputs * 3 + n_outputs * 3` gas 
per invocation, to be exact).

Functions are one of the only high-level abstractions
in Huff, so it is important to understand what the compiler adds to your code
when they are utilized. It is not always beneficial to re-use code, especially
if it is a small / inexpensive set of operations. However, for larger contracts
where certain logic is commonly reused, functions can help reduce the size of 
the contract's bytecode to below the Spurious Dragon limit.

#### Function Arguments

Functions can accept arguments to be "called" inside the macro or passed as a reference. Function arguments may be one of: label, opcode, literal, or a constant. Since functions are added to the end of the bytecode at compile-time, the arguments are not evaluated at runtime and are instead inlined as well.

#### Example

```javascript
#define macro MUL_DIV_DOWN_WRAPPER() = takes (0) returns (0) {
    0x44 calldataload // [denominator]
    0x24 calldataload // [y, denominator]
    0x04 calldataload // [x, y, denominator]
    
    // Instead of the function's code being pasted at this invocation, it is put
    // at the end of the contract's runtime bytecode and a jump to the function's
    // code as well as a jumpdest to return to is inserted here. 
    //
    // The compiler looks at the amount of stack inputs the function takes (N) and
    // holds on to an array of N SWAP opcodes in descending order from 
    // SWAP1 (0x90) + N - 1 -> SWAP1 (0x90)
    //
    // For this function invocation, we would need three swaps starting from swap3
    // and going to swap1. The return jumpdest PC must be below the function's
    // stack inputs, and the inputs still have to be in order.
    // 
    // [return_pc, x, y, denominator] (Starting stack state)
    // [denominator, x, y, return_pc] - swap3
    // [y, x, denominator, return_pc] - swap2
    // [x, y, denominator, return_pc] - swap1
    //
    // After this, the compiler inserts a jump to the jumpdest inserted at the
    // start of the function's code as well as a jumpdest to return to after
    // the function is finished executing.
    //
    // Code inserted when a function is invoked:
    // PUSH2 return_pc
    // <num_inputs swap ops>
    // PUSH2 func_start_pc
    // JUMP
    // JUMPDEST <- this is the return_pc
    MUL_DIV_DOWN(err) // [result]

    // Return result
    0x00 mstore
    0x20 0x00 return

    err:
        0x00 0x00 revert
}

#define fn MUL_DIV_DOWN(err) = takes (3) returns (1) {
    // A jumpdest opcode is inserted here by the compiler
    // Starting stack: [x, y, denominator, return_pc]

    // function code ...

    // Because the compiler knows how many stack items the function returns (N),
    // it inserts N stack swaps in ascending order from
    // SWAP1 (0x90) -> SWAP1 (0x90) + N - 1 in order to move the return_pc
    // back to the top of the stack so that it can be consumed by a JUMP
    //
    // [result, return_pc] (Starting stack state)
    // [return_pc, result] - swap1
    //
    // Final function code:
    // 👇 func_start_pc
    // JUMPDEST           [x, y, denominator, return_pc]
    // function code ...  [result, return_pc]
    // SWAP1              [return_pc, result]
    // JUMP               [result]
}
```

## Builtin Functions

Several builtin functions are provided by the Huff compiler:
#### `__FUNC_SIG(<func_def|string>)`
At compile time, the invocation of `__FUNC_SIG` is substituted with `PUSH4 function_selector`, where `function_selector` is the 4 byte function selector of the passed function definition or string. If a string is passed, it must represent a valid function signature i.e. `"test(address, uint256)"`

#### `__EVENT_HASH(<event_def|string>)`
At compile time, the invocation of `__EVENT_HASH` is substituted with `PUSH32 event_hash`, where `event_hash` is the selector hash of the passed event definition or string. If a string is passed, it must represent a valid event signature i.e. `"TestEvent(uint256, address indexed)"`

#### `__ERROR(<error_def>)`
At compile time, the invocation of `__ERROR` is substituted with `PUSH32 error_selector`, where `error_selector` is the left-padded 4 byte error selector of the passed error definition.

#### `__RIGHTPAD(<literal>)`
At compile time, the invocation of `__RIGHTPAD` is substituted with `PUSH32 padded_literal`, where `padded_literal` is the right padded version of the passed literal.

#### `__codesize(MACRO|FUNCTION)`
Pushes the code size of the macro or function passed to the stack.

#### `__tablestart(TABLE)` and `__tablesize(TABLE)`
These functions related to Jump Tables are described in the next section.

#### Example
```javascript
// Define a function
#define function test1(address, uint256) nonpayable returns (bool)
#define function test2(address, uint256) nonpayable returns (bool)

// Define an event
#define event TestEvent1(address, uint256)
#define event TestEvent2(address, uint256)

#define macro TEST1() = takes (0) returns (0) {
    0x00 0x00                // [address, uint]
    __EVENT_HASH(TestEvent1) // [sig, address, uint]
    0x00 0x00                // [mem_start, mem_end, sig, address, uint]
    log3                     // []
}

#define macro TEST2() = takes (0) returns (0) {
    0x00 0x00                // [address, uint]
    __EVENT_HASH(TestEvent2) // [sig, address, uint]
    0x00 0x00                // [mem_start, mem_end, sig, address, uint]
    log3                     // []
}

#define macro MAIN() = takes (0) returns (0) {
    // Identify which function is being called.
    0x00 calldataload 0xE0 shr
    dup1 __FUNC_SIG(test1) eq test1 jumpi
    dup1 __FUNC_SIG(test2) eq test2 jumpi

    // Revert if no function matches
    0x00 0x00 revert

    test1:
        TEST1()

    test2:
        TEST2()
}
```

## Jump Tables

Jump Tables are a convenient way to create switch cases in your
Huff contracts. Each jump table consists of jumpdest program counters (PCs), and it is
written to your contract's bytecode. These jumpdest PCs can be codecopied
into memory, and the case can be chosen by finding a jumpdest at a particular
memory pointer (i.e. 0x00 = case 1, 0x20 = case 2, etc.). This allows for a
single jump rather than many conditional jumps.

There are two different kinds of Jump Tables in Huff: `Regular` and
`Packed`. Regular Jump Tables store jumpdest PCs as full 32 byte
words, and packed Jump Tables store them each as 2 bytes. Therefore,
packed jumptables are cheaper to copy into memory, but they are more
expensive to pull a PC out of due to the bitshifting required. The
opposite is true for Regular Jump Tables.

There are two builtin functions related to jumptables.

#### `__tablestart(TABLE)`
Pushes the program counter (PC) of the start of the table passed to the stack.

#### `__tablesize(TABLE)`
Pushes the code size of the table passed to the stack.

#### Example
```javascript
// Define a function
#define function switchTest(uint256) pure returns (uint256)

// Define a jump table containing 4 pcs
#define jumptable SWITCH_TABLE {
    jump_one jump_two jump_three jump_four
}

#define macro SWITCH_TEST() = takes (0) returns (0) {
    // Codecopy jump table into memory @ 0x00
    __tablesize(SWITCH_TABLE)   // [table_size]
    __tablestart(SWITCH_TABLE)  // [table_start, table_size]
    0x00
    codecopy

    0x04 calldataload           // [input_num]

    // Revert if input_num is not in the bounds of [0, 3]
    dup1                        // [input_num, input_num]
    0x03 lt                     // [3 < input_num, input_num]
    err jumpi                       

    // Regular jumptables store the jumpdest PCs as full words,
    // so we simply multiply the input number by 32 to determine
    // which label to jump to.
    0x20 mul                    // [0x20 * input_num]
    mload                       // [pc]
    jump                        // []

    jump_one:
        0x100 0x00 mstore
        0x20 0x00 return
    jump_two:
        0x200 0x00 mstore
        0x20 0x00 return
    jump_three:
        0x300 0x00 mstore
        0x20 0x00 return
    jump_four:
        0x400 0x00 mstore
        0x20 0x00 return
    err:
        0x00 0x00 revert
}

#define macro MAIN() = takes (0) returns (0) {
    // Identify which function is being called.
    0x00 calldataload 0xE0 shr
    dup1 __FUNC_SIG(switchTest) eq switch_test jumpi

    // Revert if no function matches
    0x00 0x00 revert

    switch_test:
        SWITCH_TEST()
}
```

## Code Tables

Code Tables contain raw bytecode. The compiler places the code within
them at the end of the runtime bytecode, assuming they are referenced
somewhere within the contract.

#### Example
```javascript
#define table CODE_TABLE {
    0x604260005260206000F3
}
```

## Huff Tests

The compiler includes a simple, stripped-down testing framework to assist in creating assertions
as well as gas profiling macros and functions. huff-rs’ test suite is intentionally lacking in features,
and with that, this addition is not meant to replace developers’ dependency on `foundry-huff`. Ideally, 
for contracts that will be in production, Huff developers will utilize both foundry and Huff tests. 
If you are one of many who uses Huff as a tool for learning, Huff tests can also be a lighter weight 
experience when testing your contract’s logic.

Tests can be run via the CLI's `test` subcommand. For more information, see the [CLI Resources](../cli.md).

### Decorators
The transaction environment for each test can be modified with a decorator. Decorators sit directly above
tests, and are formatted as follows: `#[flag_a(inputs...), flag_b(inputs...)]`

Available decorators include:
* `calldata` - Set the calldata for the transaction environment. Accepts a single string of calldata bytes.
* `value` - Set the callvalue for the transaction environment. Accepts a single literal.

#### Example
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
