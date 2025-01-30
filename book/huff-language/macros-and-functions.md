# Macros and Functions

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

## Macros

Most of the time, Huff developers should opt to use macros. Each time a macro is invoked,
the code within it is placed at the point of invocation. This is efficient in
terms of runtime gas cost due to not having to jump to and from the macro's code,
but it can quickly increase the size of the contract's bytecode if it is used commonly
throughout.

### Constructor and Main

`MAIN` and `CONSTRUCTOR` are two important macros that serve special purposes. When
your contract is called, the `MAIN` macro will be the fallback, and it is commonly where
a Huff contract's control flow begins. The `CONSTRUCTOR` macro, while not required,
can be used to initialize the contract upon deployment. Inputs to the `CONSTRUCTOR` macro
are provided at compile time.

By default, the `CONSTRUCTOR` will add some bootstrap code that returns the compiled MAIN macro
as the contract's runtime bytecode. If the constructor contains a `RETURN` opcode, the compiler
will not include this bootstrap, and it will instead instantiate the contract with the code returned
by the constructor.

### Macro Arguments

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


## Functions

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

### Function Arguments

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
