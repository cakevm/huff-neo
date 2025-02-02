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

