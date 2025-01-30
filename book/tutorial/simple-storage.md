
# Simple Storage
So far the two examples we have looked at have explored slicing bytes from calldata, storing in memory and returning values. Now we're going to address the missing piece of the puzzle that all EVM devs fear, storage.

## Storage in Huff
Thankfully working with storage isn't too complicated, Huff abstracts keeping track of storage variables through the `FREE_STORAGE_POINTER()` keyword. An example of which will be shown below:

```javascript
#define constant STORAGE_SLOT0 = FREE_STORAGE_POINTER()
#define constant STORAGE_SLOT1 = FREE_STORAGE_POINTER()
#define constant STORAGE_SLOT2 = FREE_STORAGE_POINTER()
```

Storage slots are simply keys in a very large array where contracts keep their state. The compiler will assign `STORAGE_SLOT0` the value `0`, `STORAGE_SLOT1` the value `1` etc. at compile time. Throughout your code you just reference the storage slots the same way constants are used in any language.

## Setting storage

First define the constant that will represent your storage slot using the `FREE_STORAGE_POINTER()` keyword.
```javascript
#define constant VALUE = FREE_STORAGE_POINTER()
```
We can then reference this slot throughout the code by wrapping it in square brackets - like so `[VALUE]`. The example below demonstrates a macro that will store the value 5 in the slot [VALUE].
```javascript
#define macro SET_5() = takes(0) returns(0) {
    0x5             // [0x5] 
    [VALUE]         // [value_slot_pointer, 0x5]
    sstore          // []
}
```

Test this out interactively [here](https://www.evm.codes/playground?unit=Wei&codeType=Bytecode&code='6005600055'_) ([VALUE] has been hardcoded to 0)

## Reading from storage
Now you know how to write to storage, reading from storage is trivial. Simply replace `sstore` with `sload` and your ready to go. We are going to extend our example above to both write and read from storage.
```javascript
#define macro SET_5_READ_5() = takes(0) returns(0) {
    0x5
    [VALUE]
    sstore

    [VALUE]
    sload
}
```

Nice! Once again you can test this out over at [evm.codes](https://www.evm.codes/playground?unit=Wei&codeType=Bytecode&code='6005600055600054'_). Notice how 5 reappears on the stack after executing the `sload` instruction.

## Simple Storage Implementation
Now we can read and write to storage, lets attempt the famous SimpleStorage starter contract from remix.

First off, lets create our interface:
```javascript
#define function setValue(uint256) nonpayable returns ()
#define function getValue() nonpayable returns (uint256)
```

Now lets define our storage slots:
```javascript
#define constant VALUE = FREE_STORAGE_POINTER()
```

Onto the fun part, the logic. Remember from the addTwo example we can read calldata in 32 byte chunks using the `calldataload` opcode, lets use that knowledge to get read our uint256.

```javascript
#define macro SET_VALUE() = takes(0) returns(0) {
    // Read uint256 from calldata, remember to read from byte 4 to allow for the function selector! 
    0x04            // [0x04]
    calldataload    // [value]

    // Get pointer and store
    [VALUE]         // [value_ptr, value]
    sstore          // []
}
```

After completing the previous examples we hope that writing Huff is all starting to click! This pattern will be extremely common when writing your own contracts, reading from calldata then storing values in memory or storage.

Next up is reading the stored value. 
```javascript
#define macro GET_VALUE() = takes(0) returns(0) {
    // Read uint256 from storage
    [VALUE]         // [value_ptr]
    sload           // [value]

    // Store the return value in memory
    0x00            // [0x00, value]
    mstore          // []

    // Return the first 32 bytes of memory containing our uint256
    0x20            // [0x20]
    0x00            // [0x00, 0x20]
    return          // []
}
```
First off, we read the storage value using a similar technique to our prior example. Prepare the return value by storing it in memory. Then return the value from memory. It's all coming together!

To call our new macros from external functions we have to create a dispatcher! 
```javascript
#define macro MAIN() = takes(0) returns(0) {
    
    // Get the function selector
    0x00 calldataload 0xe0 shr

    dup1 __FUNC_SIG(setValue) eq setValue jumpi // Compare function selector to setValue(uint256)
    dup1 __FUNC_SIG(getValue) eq getValue jumpi // Compare the function selector to getValue()

    // dispatch
    setValue:
        SET_VALUE()
    getValue:
        GET_VALUE()

    0x00 0x00 revert
}
```

Now all of it together!
```javascript
// Interface
#define function setValue(uint256) nonpayable returns ()
#define function getValue() nonpayable returns (uint256)

// Storage
#define constant VALUE = FREE_STORAGE_POINTER()

// External function macros

// setValue(uint256)
#define macro SET_VALUE() = takes(0) returns(0) {
    // Read uint256 from calldata, remember to read from byte 4 to allow for the function selector! 
    0x04            // [0x04]
    calldataload    // [value]

    // Get pointer and store
    [VALUE]         // [value_ptr, value]
    sstore          // []
}

// getValue()
#define macro GET_VALUE() = takes(0) returns(0) {
    // Read uint256 from storage
    [VALUE]         // [value_ptr]
    sload           // [value]

    // Store the return value in memory
    0x00            // [0x00, value]
    mstore          // []

    // Return the first 32 bytes of memory containing our uint256
    0x20            // [0x20]
    0x00            // [0x00, 0x20]
    return          // []
}

// Main
#define macro MAIN() = takes(0) returns(0) {
    // Get the function selector
    0x00 calldataload 0xe0 shr

    dup1 __FUNC_SIG(setValue) eq setValue jumpi // Compare function selector to setValue(uint256)
    dup1 __FUNC_SIG(getValue) eq getValue jumpi // Compare the function selector to getValue()

    // dispatch
    setValue:
        SET_VALUE()
    getValue:
        GET_VALUE()

    0x00 0x00 revert
}
```

Congratulations! You've made it through the crust of writing contracts in Huff. For your next steps we recommend taking what you have learned so far in addTwo, "Hello, World!" and SimpleStorage into a testing framework like [Foundry](https://github.com/foundry-rs/foundry). Happy hacking!
