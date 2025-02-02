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