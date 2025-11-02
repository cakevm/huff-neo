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

### Arithmetic expressions in constants
Constants can use arithmetic expressions evaluated at compile time.

```javascript
#define constant BASE = 0x20
#define constant OFFSET = 0x04
#define constant COMBINED = BASE + OFFSET          // 0x24

#define constant WORD_SIZE = 0x20
#define constant HALF_WORD = WORD_SIZE / 0x02      // 0x10

#define constant COMPLEX = (0x0a + 0x05) * 0x02    // 0x1e
```

Supported operators: `+`, `-`, `*`, `/`, `%` (binary), `-` (unary). Parentheses `()` for grouping. See the [Constants](../huff-language/constants.md#arithmetic-expressions) documentation for details.

### New builtin constant: `__NOOP`
A special constant that generates no bytecode, useful for optional macro arguments and conditional compilation patterns.

```javascript
// Macro with optional operation
#define macro EXECUTE_OPTIONAL(op) = takes(0) returns(0) {
    <op>          // Can be __NOOP to skip this operation
    0x01 0x02 add
}

// With operation
EXECUTE_OPTIONAL(pop)  // Generates: pop, push 0x01, push 0x02, add

// Without operation (generates no extra bytecode)
EXECUTE_OPTIONAL(__NOOP)  // Generates: push 0x01, push 0x02, add
```

See the [Builtin Constants](../huff-language/builtin-constants.md) documentation for details.

### New builtin functions
There are new builtin functions available in `huff-neo`.

- `__LEFTPAD` - Left pads a hex input or the result of a passed builtin in a code table.
- `__BYTES` - Converts a string to the UTF-8 representation bytes and pushes it to the stack.
- `__ASSERT_PC` - Validates bytecode position at compile-time to ensure instructions are at expected offsets.

#### `__BYTES()` example

```javascript
#define macro MAIN() = takes (0) returns (0) {
    __BYTES("hello") // Will push UTF-8 encoded string (PUSH5 0x68656c6c6f)
}
```

#### `__ASSERT_PC()` example

Validates that bytecode is positioned at expected offsets, useful for ensuring jump destinations align correctly.

```javascript
#define constant TARGET_OFFSET = 0x20

#define macro MAIN() = takes(0) returns(0) {
    __ASSERT_PC(0x00)           // Assert we're at the start
    // ... 32 bytes of code ...
    __ASSERT_PC([TARGET_OFFSET]) // Assert we're at byte 32
    target:                      // Label for jump destination
}
```

See the [Builtin Functions](../huff-language/builtin-functions.md) documentation for complete details.

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

### Compile-time for loops
Generate repetitive code patterns at compile-time using for loops that expand before bytecode generation.

```javascript
#define constant COUNT = 10

#define macro INIT_STORAGE() = takes(0) returns(0) {
    // Initialize storage slots 0 through 9 with zero
    for(i in 0..COUNT) {
        0x00 <i> sstore
    }
}

#define macro PUSH_SEQUENCE() = takes(0) returns(5) {
    // Generate: 0x00 0x02 0x04 0x06 0x08
    for(i in 0..10 step 2) {
        <i>
    }
}
```

See the [Compile-Time Loops](../huff-language/compile-time-loops.md) documentation for complete details and examples.

### New test capabilities
The test module has been refactored to use `anvil` and `forge` features from `foundry` to fork the mainnet. This allows for more advanced testing capabilities.

