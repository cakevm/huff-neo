# Compile-Time Conditionals

Huff Neo supports compile-time `if`, `else if`, and `else` statements that expand during compilation. Conditions are evaluated at compile-time, and only the matching branch is included in the final bytecode.

## Syntax

```javascript
if (condition) {
    // statements
}

if (condition) {
    // statements
} else {
    // statements
}

if (condition) {
    // statements
} else if (condition2) {
    // statements
} else {
    // statements
}
```

## Features

- **Compile-time expansion**: Statements are expanded during compilation and don't exist in the final bytecode
- **Constant expressions**: Conditions must be constant expressions
- **Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logical NOT**: `!` operator
- **Nested conditionals**: If statements can be nested

## Basic Usage

### Simple If

```javascript
#define constant ENABLED = 0x01

#define macro MAIN() = takes(0) returns(0) {
    if ([ENABLED]) {
        0x42 0x00 mstore
    }
    // Expands to: 0x42 0x00 mstore
}
```

### If/Else

```javascript
#define constant MODE = 0x00

#define macro MAIN() = takes(0) returns(0) {
    if ([MODE]) {
        0xAA
    } else {
        0xBB
    }
    // Expands to: 0xBB
}
```

### If/Else If Chain

```javascript
#define constant VALUE = 0x02

#define macro MAIN() = takes(0) returns(0) {
    if ([VALUE] == 0x01) {
        0x11
    } else if ([VALUE] == 0x02) {
        0x22
    } else {
        0x33
    }
    // Expands to: 0x22
}
```

## Comparison Operators

Comparison operators return `1` for true, `0` for false.

### Equality

```javascript
#define constant A = 0x05
#define constant B = 0x05

if ([A] == [B]) { }  // true
if ([A] != [B]) { }  // false
```

### Relational

```javascript
#define constant X = 0x0A
#define constant Y = 0x05

if ([X] > [Y]) { }   // true (10 > 5)
if ([X] < [Y]) { }   // false
if ([X] >= [Y]) { }  // true
if ([X] <= [Y]) { }  // false
```

## Logical NOT

The `!` operator returns `1` if the operand is zero, `0` otherwise.

```javascript
#define constant DEBUG = 0x00

if (![DEBUG]) {
    // Expands (DEBUG is zero, !0 = 1)
    0xFF
}
```

## Arithmetic in Conditions

```javascript
#define constant A = 0x10
#define constant B = 0x20
#define constant THRESHOLD = 0x05

#define macro CHECK() = takes(0) returns(0) {
    if ([A] + [B] > [THRESHOLD]) {
        0xFF
    }
    // Expands to: 0xFF (0x30 > 0x05)
}
```

### With Parentheses

```javascript
#define constant A = 0x02
#define constant B = 0x03
#define constant C = 0x04

if (([A] + [B]) * [C] > 0x10) {
    // ((2 + 3) * 4) > 16
    // 20 > 16 = true
}
```

## Operator Precedence

Operators follow this precedence (highest to lowest):

1. **Parentheses**: `( )`
2. **Unary**: `-`, `!`
3. **Multiplicative**: `*`, `/`, `%`
4. **Additive**: `+`, `-`
5. **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`

Example:

```javascript
if ([A] + [B] * [C] > [D]) {
    // Evaluated as: (A + (B * C)) > D
}
```

## Nested Conditionals

```javascript
#define constant OUTER = 0x01
#define constant INNER = 0x00

#define macro NESTED() = takes(0) returns(0) {
    if ([OUTER]) {
        if ([INNER]) {
            0xAA
        } else {
            0xBB
        }
    } else {
        0xCC
    }
    // Expands to: 0xBB
}
```

## Constant References

When referencing constants in conditions, use bracket notation `[CONSTANT_NAME]`:

```javascript
#define constant FLAG = 0x01

// ✓ Correct
if ([FLAG]) { }
if ([FLAG] == 0x01) { }

// ✗ Incorrect
if (FLAG) { }
```

This follows the Huff language convention for constant values.

## Use Cases

### Feature Flags

```javascript
#define constant ENABLE_LOGGING = 0x01

#define macro PROCESS() = takes(0) returns(0) {
    if ([ENABLE_LOGGING]) {
        __EVENT_HASH("Processed()")
        0x00 0x00 log0
    }
}
```

### Configuration-Based Generation

```javascript
#define constant NETWORK = 0x01  // mainnet=1, testnet=2

#define macro GET_CHAIN_ID() = takes(0) returns(1) {
    if ([NETWORK] == 0x01) {
        0x01  // Mainnet chain ID
    } else if ([NETWORK] == 0x02) {
        0x05  // Goerli chain ID
    } else {
        0x7a69  // Local chain ID
    }
}
```

### Build Variants

```javascript
#define constant DEBUG = 0x00

#define macro MAIN() = takes(0) returns(0) {
    if ([DEBUG]) {
        __EVENT_HASH("Debug:Entry()")
        0x00 0x00 log0
    }

    PROCESS()

    if ([DEBUG]) {
        __EVENT_HASH("Debug:Exit()")
        0x00 0x00 log0
    }
}
```

## Limitations

### Constant Expressions Only

Conditions must be evaluable at compile-time:

```javascript
// ✓ Valid
if ([CONSTANT] > 5) { }

// ✗ Invalid - runtime value
if (calldataload(0x00) > 5) { }
```

### Label Scoping

Labels in one branch cannot be referenced from another:

```javascript
// ✗ Invalid
if ([FLAG]) {
    success:
        0x01
} else {
    success jump  // Error: success not visible
}
```

### No Side Effects

Conditions are pure expressions:

```javascript
// ✗ Invalid
if (some_macro()) { }
```

## Comparison with Runtime Branching

### Compile-Time (if/else)

- Conditions evaluated during compilation
- Only matching branch in bytecode
- No runtime cost
- Cannot use runtime values

```javascript
if ([CONSTANT] > 5) { }
```

### Runtime (jumpi)

- Conditions evaluated during execution
- All branches in bytecode
- Runtime gas cost
- Can use any stack values

```javascript
dup1 0x05 gt
success jumpi
// failure branch
success:
// success branch
```

## Technical Notes

- Conditions are evaluated during the expansion phase before bytecode generation
- Non-zero values are true, zero is false
- Conditions use constant folding for evaluation
- Labels follow normal scoping rules within branches

## Examples

### Multi-Level Configuration

```javascript
#define constant ENV = 0x01  // 1=prod, 2=staging, 3=dev

#define macro MAIN() = takes(0) returns(0) {
    if ([ENV] == 0x01) {
        STRICT_VALIDATE()
    } else if ([ENV] == 0x02) {
        MEDIUM_VALIDATE()
    } else {
        DEBUG_LOG()
    }
    CORE_LOGIC()
}
```

### Conditional Loop Unrolling

```javascript
#define constant SIZE = 0x04

#define macro PROCESS() = takes(0) returns(0) {
    if ([SIZE] <= 0x04) {
        // Unroll small loops
        PROCESS_ITEM(0x00)
        PROCESS_ITEM(0x01)
        PROCESS_ITEM(0x02)
        PROCESS_ITEM(0x03)
    } else {
        // Use runtime loop for larger sizes
        for(i in 0..[SIZE]) {
            PROCESS_ITEM(<i>)
        }
    }
}
```
