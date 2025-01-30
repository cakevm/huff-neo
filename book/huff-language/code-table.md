# Code Tables

Code Tables contain raw bytecode. The compiler places the code within
them at the end of the runtime bytecode, assuming they are referenced
somewhere within the contract.

## Example
```javascript
#define table CODE_TABLE {
    0x604260005260206000F3
}
```

Usage of a builtin function inside a code table:
```javascript
#define table CODE_TABLE {
    __RIGHTPAD(__FUNC_SIG("test(address, uint256)"))
    0x604260005260206000F3
}
```