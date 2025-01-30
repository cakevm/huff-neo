# Jump Tables

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

### `__tablestart(TABLE)`
Pushes the program counter (PC) of the start of the table passed to the stack.

### `__tablesize(TABLE)`
Pushes the code size of the table passed to the stack.

## Example
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