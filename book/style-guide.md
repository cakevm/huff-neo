# Style Guide

The following is a style-guide for the Huff-Std library. [Feedback and discussion](https://discord.gg/xabvMWDpEf) is welcome.

## Formatting

Lines should be no greater than 100 characters (disregarding long constant names/values).

Files should use 4 spaces for indentions.

## File Structure

The file structure of a Huff contract so follow this:

```solidity
/* Imports */
#include "./contracts/utils/ExampleImport1.huff"
#include "./contracts/utils/ExampleImport2.huff"

/* Function Interfaces */
#define function exampleFunction1(address,uint256) nonpayable returns ()
#define function exampleFunction2(address,uint256) nonpayable returns ()

#define function exampleFunction3(address,address,uint256) nonpayable returns ()
#define function exampleFunction4(address,uint256) nonpayable returns ()

#define event ExampleEvent1(address indexed example)
#define event ExampleEvent2(address indexed example)

/* Events Signatures */
#define constant EXAMPLE_EVENT_SIGNATURE1 = 0xDDF252AD1BE2C89B69C2B068FC378DAA952BA7F163C4A11628F55A4DF523B3EF
#define constant EXAMPLE_EVENT_SIGNATURE2 = 0x8C5BE1E5EBEC7D5BD14F71427D1E84F3DD0314C0F7B2291E5B200AC8C7C3B925

/* Storage Slots */
#define constant EXAMPLE_SLOT1 = FREE_STORAGE_POINTER()
#define constant EXAMPLE_SLOT2 = FREE_STORAGE_POINTER()
#define constant EXAMPLE_SLOT3 = FREE_STORAGE_POINTER()

/* Constructor */
#define macro CONSTRUCTOR() = takes(0) returns (0) {}

/* Macros */
#define macro MYMAC() = takes (0) returns (0) {}

/* Entry Point */
#define macro MAIN() = takes (0) returns (0) {}
```

The next aspect of this is more complicated, because it depends on the context and functionality of the contract. Essentially, we want to categorize macros into sections based on their functionality. Here is an example from an existing ERC20 contract:

```solidity
/* Accounting Macros */
#define macro BALANCE_OF() = takes (0) returns (0) {}
#define macro TOTAL_SUPPLY() = takes (0) returns (0) {}
#define macro ALLOWANCE() = takes (0) returns (0) {}

/* Transfer Macros */
#define macro TRANSFER_TAKE_FROM(error) = takes(3) returns (3) {}
#define macro TRANSFER_GIVE_TO() = takes(3) returns (0) {}
#define macro TRANSFER() = takes(0) returns(1) {}

/* Approval Macros */
#define macro APPROVE() = takes (0) returns (0) {}

/* Mint Macros */
#define macro MINT() = takes(0) returns (0) {}
```

## Natspec Comments

To make Huff as easy as possible to transition to from Solidity, Natspec comments are encouraged for use in the contract.

At the top of a Huff file, it is recommended to include the following natspec comments:

```md
/// @title The title of the contract
/// @author The contract author
/// @notice A short description of the contract
```

Macros are also encouraged to include natspec comments where useful. For example, huffmate's [Multicallable Huff Contract](https://github.com/pentagonxyz/huffmate/blob/main/src/utils/Multicallable.huff#L14-L21) contains the following macro natspec above the `MULTICALL()` macro.

```md
/// @notice Multicall function entry point.
/// @dev This macro should be placed alone under a function selector's jump label.
///
/// Expected calldata: `bytes[]` containing valid ABI-encoded function calls
/// as elements.
///
/// Note: this macro only allows for multicalling functions that are within
/// the contract it is invoked in.
```

## Code Comments

Comments should use the double slash (`//`) syntax, unless they are used to mark a new section of the codebase (see above).

Comments describing the functionality of a statement, macro, etc should be on the line(s) prior.

```solidity
// owner slot
#define constant OWNER_SLOT = FREE_STORAGE_POINTER()
```

Comments indicating the stack _after_ an instruction should be on the right of the instruction. Instruction comments within the same code block should be aligned vertically with the right-most instruction comment. The right-most instruction comment should be one “tab” from the right of the instruction.

```solidity
0x20    // [value]
0x00    // [offset, value]
mstore  // []
```

## Macro Definition

Macros with a non-zero `takes` expectation should include a single comment at the start of the code block indicating the expected stack in the following format.

```solidity
// Reverts if caller is not the owner.
#define macro ONLY_OWNER() = takes (1) returns (0) {
    // takes: [calling_address]
    [OWNER_SLOT]  // [owner_slot, calling_address]
    sload         // [owner_address, calling_address]
    eq            // [is_owner]
    is_owner      // [is_owner_jumpdest, is_owner]
    jumpi         // []
    0x00          // [revert_size]
    0x00          // [revert_offset, revert_size]
    revert        // []
    is_owner:     // []
}
```

The exception to this would be the function selector switch in the contract’s entry point, given its common usage.

The contract entry point should contain the function selector switch first, with each jump destination having one additional line of whitespace between one another.

```solidity
// Entry point.
#define function add(uint256, uint256) view returns (uint256)
#define function sub(uint256, uint256) view returns (uint256)

#define macro MAIN() = takes (0) returns (0) {
    // Grab the function selector from the calldata
    0x00 calldataload 0xE0 shr                 // [selector]

    dup1 __FUNC_SIG(add) eq add_func jumpi     // [selector]
    dup1 __FUNC_SIG(sub) eq sub_func jumpi     // [selector]

    // Revert if no functions match
    0x00 0x00 revert

    add_func:
        ADD()
    sub_func:
        SUB()
}
```

## Function ABI Definition

**This is entirely optional, feedback is appreciated here.**

Defining an external function ABI should consist of two components, the function definition and the 4 byte function selector defined as a constant. Defining a constant representing the function selector directly following a function definition makes for clear usage in the contract’s entry point.

The function selector definition should be screaming snake case suffixed with `_SEL`. This also makes clear the function selector switch’s behavior.

Omitting whitespace between the function definition and selector would also make the relationship between the two more clear.

```solidity
#define function add(uint256, uint256) view returns (uint256)

#define macro ADD() = takes (0) returns (0) {}

#define macro MAIN() = takes (0) returns (0) {
    // Grab the function selector from the calldata
    0x00 calldataload 0xE0 shr                 // [selector]
    dup1 __FUNC_SIG(add) eq add_func jumpi     // [selector]

    // Revert if no functions match
    0x00 0x00 revert

    add_func:
        ADD()
}
```

## Event ABI Definition

**This is entirely optional, feedback is appreciated here.**

Defining an event ABI should consist of two components, the event definition and the 32 byte event signature defined as a constant.

The event signature should be screaming snake case suffixed with `_SIG`.

Omitting whitespace between the event definition and signature would also make the relationship between the two more clear.

```solidity
#define event NewOwner(address)
#define constant NEW_OWNER_SIG = 0x3edd90e7770f06fafde38004653b33870066c33bfc923ff6102acd601f85dfbc
```

## Jump Labels

**Open to suggestions here.**

Should instructions following jump labels be indented? Doing so may make execution unclear, as the indention is not taken into consideration at compile time. In the following example, the `operation0` jump label’s subsequent code is executed, but execution continues at the `operation1` label, despite it not being clear this is the case.

```solidity
operation0
jump

// ....

operation0:
    0x01     // [b]
    0x02     // [a, b]
    add      // [sum]
operation1:
    0x02     // [b, sum]
    0x01     // [a, b, sum]
    sub      // [diff, sum]
```

It may be best to omit indentation, at least in the standard library.

```solidity
operation0
jump

// ....

operation0:
0x01         // [b]
0x02         // [a, b]
add          // [sum]
operation1:
0x02         // [b, sum]
0x01         // [a, b, sum]
sub          // [diff, sum]
```

## Contract Call Return

**Open to suggestions here.**

There is an issue with using something such as a reentrancy guard, which is a potential `return` instruction before the lock is restored to the unlocked state. Take the following example.

```solidity
#define function doAction() view returns (uint256)

#define constant LOCK_SLOT = FREE_STORAGE_POINTER()

#define macro NON_REENTRANT() = takes (0) returns (0) {
    [LOCK_SLOT]  // [lock_slot]
    sload        // [lock]
    iszero       // [is_unlocked]
    unlocked     // [unlocked_jumpdest]
    jumpi        // []
    0x00         // [size]
    0x00         // [offset, size]
    revert       // []
    unlocked:    // []
    0x01         // [lock_value]
    [LOCK_SLOT]  // [lock_slot, lock_value]
    sstore       // []
}

#define macro UNLOCK() = takes (0) returns (0) {
    0x01         // [lock_value]
    [LOCK_SLOT]  // [lock_slot, lock_value]
    sstore       // []
}

#define macro DO_ACTION() = takes (0) returns (0) {
    0x45    // [value]
    0x00    // [offset, value]
    mstore  // []
    0x20    // [size]
    0x00    // [offset, size]
    return
}

#define macro MAIN() = takes (0) returns (0) {
    // Grab the function selector from the calldata
    0x00 calldataload 0xE0 shr                       // [selector]
    dup1 __FUNC_SIG(doAction) eq do_action jumpi     // [selector]

    // Revert if no functions match
    0x00 0x00 revert

    do_action:
        NON_REENTRANT()
        DO_ACTION()
        UNLOCK()
}
```

In this example, `DO_ACTION` is non-reentrant, so it first uses `NON_REENTRANT`, but it uses the `return` instruction internally, which means the `UNLOCK` macro will never be executed, resulting in a permanently locked function.

I’m not sure the best way to handle this until a transient-storage opcode is implemented. Maybe a Huff convention could be established to have these external-function macros like `DO_ACTION` return a `size` and `offset` on the stack, then have the `return` instruction at the end.

The following would be an example of external-function macros returning the stack values instead of using the `return` instruction, allowing for more safe use of modifiers.

```solidity
#define function doAction() view returns (uint256)
#define function otherAction() view returns (uint256)

#define constant LOCK_SLOT = FREE_STORAGE_POINTER()

#define macro NON_REENTRANT() = takes (0) returns (0) {
    [LOCK_SLOT]  // [lock_slot]
    sload        // [lock]
    iszero       // [is_unlocked]
    unlocked     // [unlocked_jumpdest]
    jumpi        // []
    0x00         // [size]
    0x00         // [offset, size]
    revert       // []
    unlocked:    // []
    0x01         // [lock_value]
    [LOCK_SLOT]  // [lock_slot, lock_value]
    sstore       // []
}

#define macro UNLOCK() = takes (0) returns (0) {
    0x00         // [lock_value]
    [LOCK_SLOT]  // [lock_slot, lock_value]
    sstore       // []
}

#define macro DO_ACTION() = takes (0) returns (2) {
    0x45    // [value]
    0x00    // [offset, value]
    mstore  // []
    0x20    // [size]
    0x00    // [offset, size]
}

#define macro OTHER_ACTION() = takes (0) returns (2) {
    0x00    // [size]
    0x00    // [offset, size]
}

#define macro MAIN() = takes (0) returns (0) {
    // Grab the function selector from the calldata
    0x00 calldataload 0xE0 shr                          // [selector]
    dup1 __FUNC_SIG(doAction) eq do_action jumpi        // [selector]
    dup1 __FUNC_SIG(otherAction) eq other_action jumpi  // [selector]

    // Revert if no functions match
    0x00 0x00 revert


    do_action:
        NON_REENTRANT()
        DO_ACTION()
        finish jump

    other_action:
        OTHER_ACTION()
        finish jump

    finish:
        // stack: [offset, size]
        UNLOCK()
        return
}
```
