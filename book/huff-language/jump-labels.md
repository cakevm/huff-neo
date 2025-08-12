# Jump Labels

Jump Labels are a simple abstraction included into the language to make defining
and referring to `JUMPDEST`s more simple for the developer.

## Example

```javascript
#define macro MAIN() = takes (0) returns (0) {
    // Store "Hello, World!" in memory
    0x48656c6c6f2c20576f726c6421
    0x00 mstore // ["Hello, World!"]

    // Jump to success label, skipping the revert statement
    success     // [success_label_pc, "Hello, World!"]
    jump        // ["Hello, World!"]

    // Revert if this point is reached
    0x00 0x00 revert

    // Labels are defined within macros or functions, and are designated
    // by a word followed by a colon. Note that while it may appear as if
    // labels are scoped code blocks due to the indentation, they are simply
    // destinations to jump to in the bytecode. If operations exist below a label,
    // they will be executed unless the program counter is altered or execution is
    // halted by a `revert`, `return`, `stop`, or `selfdestruct` opcode.
    success:
        0x00 mstore
        0x20 0x00 return
}
```

## Label Scoping

Huff-Neo implements scoped label resolution to prevent situations where multiple macro invocations defining the same label would cause all jumps to incorrectly target the last definition.

### Scoping Rules

#### Label Definition
- Each label is associated with the scope where it's defined
- A scope is determined by the macro invocation chain
- Labels defined directly in a macro belong to that macro's scope
- Labels defined in invoked macros belong to the invoked macro's scope

#### Duplicate Detection
- **Within Same Scope**: Duplicate labels in the same scope will cause a compilation error
- **Across Different Scopes**: Labels with the same name in different scopes are allowed (shadowing)

#### Label Resolution
- When a jump references a label, it resolves to the nearest matching label in the scope hierarchy
- Resolution searches from the current scope upward to parent scopes
- Inner scope labels shadow outer scope labels with the same name

### Examples

#### Duplicate Labels (Error)
```huff
#define macro MAIN() = takes(0) returns(0) {
    my_label:       // First definition
    0x01
    my_label:       // ERROR: Duplicate label in same scope
    0x02
}
```

#### Label Shadowing (Allowed)
```huff
#define macro INNER() = takes(0) returns(0) {
    my_label:       // Label in INNER scope
    0x01
    my_label        // Jumps to INNER's my_label
}

#define macro MAIN() = takes(0) returns(0) {
    my_label:       // Label in MAIN scope
    0x00
    INNER()         // INNER has its own my_label
    my_label        // Jumps to MAIN's my_label
}
```

#### Multiple Macro Invocations
```huff
#define macro DEF_LBL() = takes(0) returns(0) {
    my_label:
    0x42
    my_label        // Jumps to THIS invocation's my_label
}

#define macro MAIN() = takes(0) returns(0) {
    DEF_LBL()       // First invocation - creates my_label in its scope
    DEF_LBL()       // Second invocation - creates my_label in its own scope
    // Both invocations work correctly with their own labels
}
```

### Best Practices

1. Use unique label names within a macro to avoid confusion
2. Be aware that macro invocations create new scopes
3. Use label shadowing intentionally and document it when used
4. Consider prefixing labels with macro names for clarity in complex contracts