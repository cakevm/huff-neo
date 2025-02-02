# Constants

Constants in Huff contracts are not included in the contract's storage; Instead,
they are able to be called within the contract at compile time. Constants
can either be bytes (32 max), `FREE_STORAGE_POINTER` or a built-in function. A `FREE_STORAGE_POINTER`
constant will always represent an unused storage slot in the contract.

In order to push a constant to the stack, use bracket notation: `[CONSTANT]`

## Example

**Constant Declaration**
```javascript
#define constant NUM = 0x420
#define constant HELLO_WORLD = 0x48656c6c6f2c20576f726c6421
#define constant FREE_STORAGE = FREE_STORAGE_POINTER()
#define constant TEST = __FUNC_SIG("test(uint256)")
```

**Constant Usage**
(without loss of generality, let's say the constant `NUM` holds 0x420 from the above example)
```plaintext
                    // [] - an empty stack
[NUM]               // [0x420] - the constant's value is pushed to the stack
```