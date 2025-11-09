# Code Tables

Code Tables contain raw bytecode. The compiler places the code within
them at the end of the runtime bytecode, assuming they are referenced
somewhere within the contract.

## Example

### Define a code table
```javascript
#define table CODE_TABLE {
    0x604260005260206000F3
}

#define macro MAIN() = takes (0) returns (0) {
    
    // Copy the code table into memory at 0x00
    __tablesize(CODE_TABLE)   // size
    __tablestart(CODE_TABLE)  // offset
    0x00                      // destOffset
    codecopy
}
```

### Usage of a builtin function
```javascript
#define table CODE_TABLE {
    __RIGHTPAD(__FUNC_SIG("test(address, uint256)"))
    0x604260005260206000F3
}
```

### Usage of a constant reference
```javascript
#define constant CONST = 0x123

#define table CODE_TABLE {
    [CONST]
    __LEFTPAD([CONST])
}
```

### Usage of builtin function constants
```javascript
#define constant SIG = __FUNC_SIG("transfer(address,uint256)")
#define constant PADDED = __RIGHTPAD(0x42)

#define table CODE_TABLE {
    [SIG]       // Function selector from constant
    [PADDED]    // Padded value from constant
}
```

This is particularly useful when you want to reuse computed builtin values in multiple places, including both code and data tables.

