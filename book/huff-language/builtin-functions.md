# Builtin Functions

Several builtin functions are provided by the Huff compiler. The functions can be used inside a macro, function definition to generate the desired bytecode. The usage of `__RIGHTPAD`, `__FUNC_SIG` and `__BYTES` is usable in a data table. The builtin functions are evaluated at compile time and substituted with the corresponding bytecode.


### `__FUNC_SIG(<string>|<function definition>)`
At compile time, the invocation of `__FUNC_SIG` is substituted with `PUSH4 function_selector`, where `function_selector` is the 4 byte function selector of the passed function definition or string. If a string is passed, it must represent a valid function signature i.e. `"test(address, uint256)"`

### `__EVENT_HASH(<string>|<event definition>)`
At compile time, the invocation of `__EVENT_HASH` is substituted with `PUSH32 event_hash`, where `event_hash` is the selector hash of the passed event definition or string. If a string is passed, it must represent a valid event signature i.e. `"TestEvent(uint256, address indexed)"`

### `__ERROR(<error definition>)`
At compile time, the invocation of `__ERROR` is substituted with `PUSH32 error_selector`, where `error_selector` is the left-padded 4 byte error selector of the passed error definition.

### `__LEFTPAD(<string>|<hex>|<builtin function>)`
At compile time, the invocation of `__LEFTPAD` is substituted with `padded_literal`, where `padded_literal` is the left padded version of the passed input. This function is only available as constant assignment or in a code table.

#### Example
```plaintext
__LEFTPAD(0x123)
// will result in 32 bytes:
0x0000000000000000000000000000000000000000000000000000000000000123
```

### `__RIGHTPAD(<string>|<hex>|<builtin function>)`
At compile time, the invocation of `__RIGHTPAD` is substituted with `PUSH32 padded_literal`, where `padded_literal` is the right padded version of the passed input.

#### Example
```plaintext
__RIGHTPAD(0x123)
// will result in 32 bytes:
0x1230000000000000000000000000000000000000000000000000000000000000
```

### `__codesize(<macro>|<function>)`
Pushes the code size of the macro or function passed to the stack.

### `__tablestart(<table>)` and `__tablesize(<table>)`
These functions related to Jump Tables are described in the next section.

### `__VERBATIM(<hex>)`
This function is used to insert raw hex data into the compiled bytecode. It is useful for inserting raw opcodes or data into the compiled bytecode.

### `__BYTES(<string>)`
This function allows to insert a string as UTF-8 encoded bytes into the compiled bytecode. The resulting bytecode is limited to 32 bytes.

#### Example
```javascript
#define macro MAIN() = takes (0) returns (0) {
    __BYTES("hello") // Will push UTF-8 encoded string (PUSH5 0x68656c6c6f)
}
```

### `__ASSERT_PC(<literal>|<constant>)`
This function is a compile-time assertion that validates the current bytecode position (program counter) matches the expected value. It generates no bytecode - it's purely a compile-time check. If the assertion fails, compilation will stop with an error showing the expected vs actual position.

This is useful for ensuring critical instructions (like jump destinations) are positioned at specific bytecode offsets, which can be important for gas optimization or when interfacing with external systems that expect specific bytecode layouts.

#### Example
```javascript
#define macro MAIN() = takes (0) returns (0) {
    __ASSERT_PC(0x00)    // Assert we're at the start
    0x01 0x02            // PUSH1 0x01, PUSH1 0x02 = 4 bytes
    __ASSERT_PC(0x04)    // Assert we're at byte 4
    target:              // Label at position 0x04 (generates JUMPDEST = 1 byte)
    __ASSERT_PC(0x05)    // Assert we're at byte 5
    0x11                 // PUSH1 0x11 = 2 bytes
    __ASSERT_PC(0x07)    // Assert we're at byte 7
}
```

You can also use constants and combine with for-loops:
```javascript
#define constant TARGET_OFFSET = 0x20

#define macro MAIN() = takes (0) returns (0) {
    // Fill space to reach target offset using for-loop with constant
    for(i in 0..[TARGET_OFFSET]) {
        stop  // Expands to 32 stop instructions = 32 bytes
    }
    __ASSERT_PC([TARGET_OFFSET])  // Ensure we're at the expected offset (0x20 = 32 bytes)
    important_label:             // Label generates JUMPDEST automatically
}
```

## Combining Builtin Functions
Some builtin functions can be combined with other functions. Possible combinations are:
- `__RIGHTPAD(__FUNC_SIG("test(address, uint256)"))`
- `__RIGHTPAD(__BYTES("hello"))`


## Example
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

## See Also

- **[Constants](./constants.md#free_storage_pointer)** - Documentation for `FREE_STORAGE_POINTER()`, a special compile-time function that allocates unique storage slots
- **[Builtin Constants](./builtin-constants.md)** - Documentation for built-in constants like `__NOOP`