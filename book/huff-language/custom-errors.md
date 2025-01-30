# Custom Errors

Custom errors can be defined and used by the `__ERROR` builtin to push the left-padded 4 byte
error selector to the stack.

## Example
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