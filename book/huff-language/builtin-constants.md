# Builtin Constants

Built-in constants are compiler-provided constants with special behavior during compilation. Unlike user-defined constants which resolve to literal values, built-in constants may generate specialized bytecode or no bytecode.

## `__NOOP`

At compile time, `__NOOP` generates no bytecode. When the compiler encounters `__NOOP`, it skips code generation for that position.

### Behavior

- `__NOOP` can appear in macro bodies, as macro arguments, in for loop bodies, and inside label definitions
- When referenced through constants (`[CONST]`), it generates no bytecode
- `__NOOP` is a reserved name and cannot be used for user-defined constants

### Example: Basic Usage

```javascript
#define macro EXAMPLE() = takes(0) returns(0) {
    __NOOP       // Generates: (nothing)
    0x01         // Generates: PUSH1 0x01 = 6001
    __NOOP       // Generates: (nothing)
    dup1         // Generates: DUP1 = 80
    __NOOP       // Generates: (nothing)
}
// Total bytecode: 600180
```

### Example: As a Named Constant

```javascript
#define constant NO_OP = __NOOP

#define macro MAIN() = takes(0) returns(0) {
    [NO_OP]  // Generates no bytecode
    0x42     // Generates: PUSH1 0x42
}
```

### Example: Conditional Code via Macro Arguments

```javascript
#define macro OPTIONAL_LOG(value, should_log) = takes(0) returns(0) {
    <should_log>  // Expands to opcodes or __NOOP
    <value>
}

#define macro MAIN() = takes(0) returns(0) {
    // With logging
    OPTIONAL_LOG(0xff, log0)  // Generates: LOG0 PUSH1 0xff

    // Without logging
    OPTIONAL_LOG(0xff, __NOOP)  // Generates: PUSH1 0xff
}
```

## See Also

- [Builtin Functions](./builtin-functions.md) - Compile-time functions like `__FUNC_SIG()` and `__TABLESIZE()`
- [Constants](./constants.md) - User-defined constants and `FREE_STORAGE_POINTER()`
- [Macros and Functions](./macros-and-functions.md) - How to use `__NOOP` with macro arguments
