## There is `huff-rs` and `huff-neo`?

Yes, there are two versions of the Huff compiler. The original compiler, `huff-rs` is no longer maintained. There is a plan to revive it as [huff2](https://github.com/huff-language/huff2), but since it is a new development from scratch, it is unknown when it will have feature parity with the original compiler. `huff-neo` tries to step-in and continues the development and also tries to evolve the Huff language where necessary.

## New in `huff-neo`
There are some new features in `huff-neo` that are not available in `huffc`.

### Allowed use of built-in functions for constants and code tables
The usage of built-in functions for constants and code tables is now possible.

```javascript
#define constant FUNC_TEST = __FUNC_SIG("test(uint256)")
#define constant BYTES_HELLO = __BYTES("hello")

#define code_table TEST_TABLE {
    0x123
    __FUNC_SIG("test(uint256)")
    __BYTES("hello")
    __LEFTPAD(__FUNC_SIG("test(uint256)")) // New built-in function __LEFTPAD
    [FUNC_TEST]
    [BYTES_HELLO]
}
```

### New built-in functions
There are new built-in functions available in `huff-neo`.

- `__LEFTPAD` - Left pads a hex input or the result of a passed built-in in a code table.
- `__BYTES` - Converts a string to the UTF-8 representation bytes and pushes it to the stack.

```javascript
#define macro MAIN() = takes (0) returns (0) {
    __BYTES("hello") // Will push UTF-8 encoded string (PUSH5 0x68656c6c6f)
}
```

### First-class macros
Macros can now be passed as arguments to other macros and invoked dynamically, making code more reusable and flexible.

```javascript
#define macro ADD(a, b) = takes(0) returns(1) {
    <a> <b> add
}

#define macro APPLY(fn, x, y) = takes(0) returns(1) {
    <fn>(<x>, <y>)  // Invoke macro with arguments
}

#define macro MAIN() = takes(0) returns(0) {
    APPLY(ADD, 0x05, 0x06)  // Expands to: 0x05 0x06 add
}
```

See the [Macros and Functions](../huff-language/macros-and-functions.md#first-class-macro-arguments) documentation for more details.

### New test capabilities
The test module has been refactored to use `anvil` and `forge` features from `foundry` to fork the mainnet. This allows for more advanced testing capabilities.

