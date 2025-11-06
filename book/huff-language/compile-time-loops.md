# Compile-Time Loops

Huff Neo supports compile-time `for` loops that expand during compilation, allowing you to generate repetitive code patterns without manual duplication.

## Syntax

```javascript
for(variable in start..end) {
    // loop body
}

for(variable in start..end step N) {
    // loop body with custom step
}
```

## Features

- **Compile-time expansion**: Loops are expanded during compilation and don't exist in the final bytecode
- **Variable interpolation**: Use `<variable>` to insert the current iteration value as a literal
- **Constant expressions**: Loop bounds can use constants and arithmetic
- **Nested loops**: Loops can be nested within each other

## Basic Usage

### Simple Repetition

Generate a sequence of literal values:

```javascript
#define macro PUSH_SEQUENCE() = takes(0) returns(3) {
    for(i in 0..3) {
        <i>
    }
    // Expands to: 0x00 0x01 0x02
}
```

### With Opcodes

Use loop variables with explicit opcodes:

```javascript
#define macro PUSH_VALUES() = takes(0) returns(3) {
    for(i in 1..4) {
        push1 <i>
    }
    // Expands to: push1 0x01 push1 0x02 push1 0x03
}
```

### Custom Step

Use a step value other than 1:

```javascript
#define macro EVEN_NUMBERS() = takes(0) returns(5) {
    for(i in 0..10 step 2) {
        <i>
    }
    // Expands to: 0x00 0x02 0x04 0x06 0x08
}
```

## Variable Interpolation

The `<variable>` syntax inserts the current iteration value as a hex literal (matching Huff's existing `<arg>` syntax for macro arguments):

```javascript
for(i in 0..3) {
    <i>  // Becomes: 0x00, then 0x01, then 0x02
}
```

**Note**: Loop variables shadow macro arguments. Inside a for loop, `<i>` always refers to the loop variable, even if there's a macro argument with the same name.

### Limitations

- `<variable>` only works as a **literal value**
- Cannot interpolate into identifiers or label names
- Cannot use variable in constant names

**Supported**:
```javascript
for(i in 0..3) {
    <i>        // ✓ Literal value
    push1 <i>  // ✓ Literal after opcode
}
```

**Not Supported**:
```javascript
for(i in 0..3) {
    [SLOT_<i>]   // ✗ No interpolation in identifiers
    label_<i>:   // ✗ No interpolation in labels
}
```

## Loop Bounds

Loop bounds must be constant expressions that can be evaluated at compile-time.

### Constant References

When referencing constants in loop bounds or step values, you **must** use bracket notation `[CONSTANT_NAME]`:

```javascript
#define constant END = 10

// ✓ Correct - bracket notation required
for(i in 0..[END]) { }

// ✗ Incorrect - bare constant name not allowed
for(i in 0..END) { }
```

This applies to all positions where constants appear:
- Start bound: `for(i in [START]..end)`
- End bound: `for(i in start..[END])`
- Step value: `for(i in start..end step [STEP])`

### U256 Range Support

Loop ranges support **full u256 values** using hex notation:

```javascript
#define constant START = 0x00
#define constant END = 0xFF
#define constant STEP = 0x10

#define macro USE_CONSTANTS() = takes(0) returns(16) {
    for(i in [START]..[END] step [STEP]) {
        <i>
    }
}
```

You can use hex literals directly:

```javascript
for(i in 0x00..0x100 step 0x20) {
    <i>
}
// Works with any u256 value up to 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
```

### Arithmetic in Bounds

You can use arithmetic expressions:

```javascript
#define constant SIZE = 5

for(i in 0..[SIZE] * 2) {
    <i>
}
// Expands iterations from 0 to 9
```

### Macro Arguments in Bounds

Macro arguments can be used in loop bounds using `<arg>` syntax. The arguments are evaluated at compile-time during macro expansion.

#### Basic Usage

```javascript
#define macro LOOP(count) = takes(0) returns(0) {
    for(i in 0..<count>) {
        <i>
    }
}

#define macro MAIN() = takes(0) returns(0) {
    LOOP(0x03)  // Expands to: 0x00 0x01 0x02
}
```

#### With Start and End Arguments

```javascript
#define macro RANGE(start, end) = takes(0) returns(0) {
    for(i in <start>..<end>) {
        <i>
    }
}

#define macro MAIN() = takes(0) returns(0) {
    RANGE(0x05, 0x08)  // Expands to: 0x05 0x06 0x07
}
```

#### With Step Argument

```javascript
#define macro STEPPED(count, step) = takes(0) returns(0) {
    for(i in 0..<count> step <step>) {
        <i>
    }
}

#define macro MAIN() = takes(0) returns(0) {
    STEPPED(0x0A, 0x02)  // Expands to: 0x00 0x02 0x04 0x06 0x08
}
```

#### With Arithmetic

Arguments can be used in arithmetic expressions:

```javascript
#define macro COMPUTED(base, extra) = takes(0) returns(0) {
    for(i in 0..(<base> + <extra>)) {
        <i>
    }
}

#define macro MAIN() = takes(0) returns(0) {
    COMPUTED(0x02, 0x03)  // Loop from 0 to 5
}
```

#### Mixed with Constants

Arguments can be combined with constants in bound expressions:

```javascript
#define constant BASE = 0x10

#define macro ADD_RANGE(offset) = takes(0) returns(0) {
    for(i in [BASE]..([BASE] + <offset>)) {
        <i>
    }
}

#define macro MAIN() = takes(0) returns(0) {
    ADD_RANGE(0x05)  // Loop from 0x10 to 0x15
}
```

### Iteration Limit

**Important**: To prevent compile-time explosion and excessive bytecode generation, loops are limited to **10,000 iterations maximum**.

If your loop exceeds this limit, you'll get a compile error:

```javascript
// This will error - exceeds 10,000 iterations
for(i in 0..0x10000) {  // 65,536 iterations
    <i>
}
```

This safety limit prevents:
- Runaway compilation times
- Out-of-memory errors
- Accidentally generating massive bytecode

## Nested Loops

Loops can be nested for multi-dimensional patterns:

```javascript
#define macro NESTED_EXAMPLE() = takes(0) returns(0) {
    for(i in 0..2) {
        for(j in 0..2) {
            // Both i and j are available
            <i>
            <j>
        }
    }
    // Expands to: 0x00 0x00 0x00 0x01 0x01 0x00 0x01 0x01
}
```

## Common Use Cases

### Initialize Storage Slots

```javascript
#define macro INIT_STORAGE() = takes(0) returns(0) {
    for(slot in 0..10) {
        0x00 <slot> sstore  // Store zero at each slot
    }
}
```

### Generate Function Selector Table

```javascript
#define macro SELECTOR_TABLE() = takes(0) returns(0) {
    for(idx in 0..5) {
        dup1
        <idx>
        eq
        handler jumpi
    }
}
```

### Unroll Loops for Gas Optimization

Instead of runtime loops, unroll them at compile-time:

```javascript
// Runtime loop (more gas per iteration)
loop:
    // ...
    loop jumpi

// Compile-time loop (no runtime overhead)
for(i in 0..[COUNT]) {
    // ... unrolled code
}
```

## Technical Notes

- Loop bounds are inclusive of `start`, exclusive of `end` (like Rust ranges)
- Default step is 1 if not specified
- Step must not be zero (compile error)
- Labels inside loop bodies are automatically renamed to prevent conflicts
- Maximum iteration count limited by reasonable compile-time bounds

## Examples

### Matrix Operation

```javascript
#define constant MATRIX_SIZE = 4

#define macro INIT_MATRIX() = takes(0) returns(0) {
    for(row in 0..[MATRIX_SIZE]) {
        for(col in 0..[MATRIX_SIZE]) {
            0x00
            <row>
            <col>
            add
            sstore
        }
    }
}
```

### Optimized Array Copy

```javascript
#define constant ARRAY_LENGTH = 32

#define macro COPY_ARRAY() = takes(2) returns(0) {
    // Takes: source_ptr dest_ptr
    for(offset in 0..[ARRAY_LENGTH] step 0x20) {
        // Load from source
        dup2 <offset> add mload
        // Store to dest
        dup2 <offset> add mstore
    }
    pop pop
}
```
