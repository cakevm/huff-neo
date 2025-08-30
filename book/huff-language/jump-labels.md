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
Labels are resolved in the following order:
1. **Current and Parent Scopes**: Searches from the current scope upward through parent scopes
2. **Child Scopes**: If not found, searches in child scopes (macros invoked from current scope)
3. **Sibling Scopes**: As a fallback, searches in sibling scopes (macros invoked from the same parent)

This resolution order allows:
- Inner scope labels to shadow outer scope labels
- Parent macros to reference labels defined in nested child macros
- Sibling macros to cross-reference each other's labels when needed

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

#### Parent Accessing Child Labels
```huff
#define macro INNER() = takes(0) returns(0) {
    inner_label:    // Label defined in nested macro
    0x42
}

#define macro OUTER() = takes(0) returns(0) {
    INNER()         // Invoke macro that defines inner_label
}

#define macro MAIN() = takes(0) returns(0) {
    OUTER()
    inner_label jump // Parent can jump to label in nested child
}
```

#### Sibling Cross-References
```huff
#define macro FIRST() = takes(0) returns(0) {
    first_label:
    0x01
}

#define macro SECOND() = takes(0) returns(0) {
    first_label jump // Can reference sibling's label
}

#define macro MAIN() = takes(0) returns(0) {
    FIRST()         // Defines first_label
    SECOND()        // References first_label from FIRST
}
```

#### Labels as Macro Arguments

When labels are passed as arguments to macros, they are resolved from the perspective of the macro that's passing them, not the macro that's using them. This allows labels to be accessed across different scopes:

```huff
#define macro SET_LABEL() = takes(0) returns(0) {
    my_label:
    0x42
}

#define macro USE_LABEL(lbl) = takes(0) returns(0) {
    <lbl> jump      // Uses the label passed as argument
}

#define macro WRAPPER(m) = takes(0) returns(0) {
    USE_LABEL(<m>)  // Pass label as argument to USE_LABEL
}

#define macro MAIN() = takes(0) returns(0) {
    SET_LABEL()         // Define my_label in SET_LABEL's scope
    WRAPPER(my_label)   // Pass label through nested invocations
}
```

In this example:
1. `my_label` is defined in `SET_LABEL`'s scope (a child of MAIN)
2. When `MAIN` calls `WRAPPER(my_label)`, the label is resolved from MAIN's perspective
3. MAIN can see `my_label` because it's defined in a child scope (SET_LABEL)
4. `WRAPPER` passes the label to `USE_LABEL` as `<m>`
5. When `USE_LABEL` uses `<lbl>`, it jumps to the correct label

This works even with deeply nested invocations:

```huff
#define macro DEEP_USE(label) = takes(0) returns(0) {
    <label> jump
}

#define macro LEVEL2(lbl) = takes(0) returns(0) {
    DEEP_USE(<lbl>)     // Pass through another level
}

#define macro LEVEL1(l) = takes(0) returns(0) {
    LEVEL2(<l>)         // Pass through to LEVEL2
}

#define macro MAIN() = takes(0) returns(0) {
    target:
    0x42
    LEVEL1(target)      // Label resolved from MAIN's scope
}
```

The key point is that label arguments (`<label>`) are resolved where they are passed, not where they are used. This enables flexible label passing between macros at different scope levels.

##### Label Shadowing with Arguments

When passing labels as arguments, shadowing rules still apply. Inner scope labels shadow outer scope labels:

```huff
#define macro USE_LABEL(lbl) = takes(0) returns(0) {
    <lbl> jump
}

#define macro INNER() = takes(0) returns(0) {
    target:             // Inner scope's target
    0x01
    USE_LABEL(target)   // Resolves to INNER's target (0x01)
}

#define macro MAIN() = takes(0) returns(0) {
    target:             // MAIN's target
    0x00
    INNER()             // INNER defines its own target
    USE_LABEL(target)   // Resolves to MAIN's target (0x00)
}
```

In this example:
- `INNER` defines its own `target` label that shadows `MAIN`'s `target`
- When `INNER` passes `target` to `USE_LABEL`, it resolves to INNER's label
- When `MAIN` passes `target` to `USE_LABEL`, it resolves to MAIN's label
- Each macro sees and passes its own version of the label

This shadowing behavior is useful when you want macros to work with their local labels without worrying about naming conflicts:

```huff
#define macro JUMP_TO(lbl) = takes(0) returns(0) {
    <lbl> jump
}

#define macro PROCESS() = takes(0) returns(0) {
    loop:               // Local loop label
    0x01 add
    dup1 0x10 lt
    loop JUMP_TO        // Uses local loop, not affected by other loops
}

#define macro MAIN() = takes(0) returns(0) {
    loop:               // MAIN's loop label
    PROCESS()           // PROCESS has its own loop
    loop jump           // Jumps to MAIN's loop
}
```

### Best Practices

1. Use unique label names within a macro to avoid confusion
2. Be aware that macro invocations create new scopes
3. Use label shadowing intentionally and document it when used
4. Consider prefixing labels with macro names for clarity in complex contracts