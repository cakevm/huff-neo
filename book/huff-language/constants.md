# Constants

Constants in Huff contracts are not included in the contract's storage; Instead,
they are able to be called within the contract at compile time. Constants
can be bytes (32 max), string literals, `FREE_STORAGE_POINTER`, a built-in function, or an arithmetic expression.
A `FREE_STORAGE_POINTER` constant will always represent an unused storage slot in the contract.

In order to push a constant to the stack, use bracket notation: `[CONSTANT]`

## Example

**Constant Declaration**
```javascript
#define constant NUM = 0x420
#define constant HELLO_WORLD = 0x48656c6c6f2c20576f726c6421
#define constant GREETING = "hello"
#define constant FREE_STORAGE = FREE_STORAGE_POINTER()
#define constant TEST = __FUNC_SIG("test(uint256)")
```

**Constant Usage**
(without loss of generality, let's say the constant `NUM` holds 0x420 from the above example)
```plaintext
                    // [] - an empty stack
[NUM]               // [0x420] - the constant's value is pushed to the stack
```

## Arithmetic Expressions

Constants can use arithmetic expressions evaluated at compile time.

### Supported Operators

**Binary Operators:**
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division (integer)
- `%` Modulo

**Unary Operators:**
- `-` Negation

### Operator Precedence

Operators follow standard mathematical precedence:

1. Unary negation (`-`), parentheses `()`
2. Multiplication (`*`), Division (`/`), Modulo (`%`) - left-associative
3. Addition (`+`), Subtraction (`-`) - left-associative

Use parentheses to override precedence.

### Examples

**Basic Arithmetic:**
```javascript
#define constant BASE = 0x20
#define constant OFFSET = 0x04
#define constant COMBINED = [BASE] + [OFFSET]      // Result: 0x24

#define constant TEN = 0x0a
#define constant FIVE = 0x05
#define constant SIMPLE_ADD = [TEN] + [FIVE]        // Result: 0x0f
#define constant SIMPLE_SUB = [TEN] - [FIVE]        // Result: 0x05

#define constant MULTIPLY = 0x03 * 0x04             // Result: 0x0c
#define constant DIVIDE = 0x10 / 0x02               // Result: 0x08
#define constant MODULO = 0x0a % 0x03               // Result: 0x01
```

**Complex Expressions:**
```javascript
#define constant COMPLEX = ([TEN] + [FIVE]) * 0x02 // Result: 0x1e (30 in decimal)
#define constant NESTED = [BASE] + ([OFFSET] * 0x02) // Result: 0x28

// Precedence: multiplication before addition
#define constant PRECEDENCE = 0x02 + 0x03 * 0x04   // Result: 0x0e (2 + 12)

// Left-associative operations
#define constant CHAIN = 0x10 - 0x05 - 0x02         // Result: 0x09 ((16 - 5) - 2)
```

**Negation:**
```javascript
#define constant POSITIVE = 0x0a
#define constant NEGATIVE = -[POSITIVE]             // Result: 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6

// Negation in expressions
#define constant MIXED = -0x05 + 0x0a               // Result: 0x05
```

### Use Cases

**Storage Slots:**
```javascript
#define constant STORAGE_SLOT_0 = 0x00
#define constant STORAGE_SLOT_1 = [STORAGE_SLOT_0] + 0x01
#define constant STORAGE_SLOT_2 = [STORAGE_SLOT_0] + 0x02
```

**Memory Offsets:**
```javascript
#define constant MEM_OFFSET_0 = 0x00
#define constant MEM_OFFSET_32 = [MEM_OFFSET_0] + 0x20
#define constant MEM_OFFSET_64 = [MEM_OFFSET_32] + 0x20
```

**Size Computations:**
```javascript
#define constant WORD_SIZE = 0x20
#define constant HALF_WORD = [WORD_SIZE] / 0x02
#define constant DOUBLE_WORD = [WORD_SIZE] * 0x02
```

**ABI Encoding:**
```javascript
#define constant SELECTOR_SIZE = 0x04
#define constant FIRST_ARG_OFFSET = SELECTOR_SIZE
#define constant SECOND_ARG_OFFSET = [SELECTOR_SIZE] + 0x20
```

### Compile-Time Evaluation

Expressions are evaluated during compilation. The compiler replaces the expression with the computed value and generates a single `PUSH` instruction. Constants must be defined before use.

```javascript
#define constant A = 0x10
#define constant B = 0x05
#define constant C = [A] + [B]  // Evaluated to 0x15 at compile time

#define macro EXAMPLE() = takes(0) returns(0) {
    [C]    // Compiles to: PUSH1 0x15
}
```

## String Literals

String literal constants can be defined using double quotes. String constants cannot be pushed to the stack directly - they must be converted to bytes using the `__BYTES` builtin function.

### String Constant Declaration

```javascript
#define constant GREETING = "hello"
#define constant MESSAGE = "transfer(address,uint256)"
```

### String Constant Usage

String constants must be used with the `__BYTES` builtin function, which converts the string to UTF-8 encoded bytes:

```javascript
#define constant GREETING = "hello"

#define macro MAIN() = takes(0) returns(0) {
    __BYTES([GREETING])  // Compiles to: PUSH5 0x68656c6c6f
}
```

**Direct usage is not allowed:**
```javascript
// ❌ ERROR: String constants cannot be pushed directly
#define constant MSG = "hello"

#define macro EXAMPLE() = takes(0) returns(0) {
    [MSG]  // Compilation error: use __BYTES([MSG]) instead
}
```

### Use Cases

**Function Signatures:**
```javascript
#define constant TRANSFER_SIG = "transfer(address,uint256)"

#define macro GET_SELECTOR() = takes(0) returns(1) {
    __BYTES([TRANSFER_SIG])  // Push signature as bytes
}
```

**Error Messages (limited to 32 bytes):**
```javascript
#define constant ERROR_MSG = "Insufficient balance"

#define macro REVERT_WITH_MSG() = takes(0) returns(0) {
    __BYTES([ERROR_MSG])
    0x00 mstore
    0x20 0x00 revert
}
```

**Combining with Other Builtins:**
```javascript
#define constant GREETING = "hi"

#define macro EXAMPLE() = takes(0) returns(0) {
    // String bytes can be padded
    __RIGHTPAD(__BYTES([GREETING]))  // Right-pad the UTF-8 bytes
}
```