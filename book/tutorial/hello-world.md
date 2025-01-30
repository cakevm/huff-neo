# Hello, world!

Note: This section is a continuation of ['The Basics'](the-basics.md). If you have not read it yet we recommend you take a look at it before continuing!

---

Within ['The Basics'](the-basics.md) we mentioned how `"Hello, world!"` is quite an advanced concept for Huff. The reason being that we need to have an understanding of how strings are encoded in the EVM.

## Primer: ABI Encoding

As strings are dynamic types it is not as simple as returning the UTF-8 values for `"Hello, world!"` (`0x48656c6c6f2c20776f726c6421`). In the ABI standard, dynamic types are encoded in 3 parts, each which takes a full word (32 bytes) of memory.

1. The offset of the dynamic data. (pointer to the start of the dynamic data, uint256)
2. The length of the dynamic data. (uint256)
3. The values of the dynamic data. (dynamic length)

Each part will look as follows for the string `"Hello, world!"`:

```plaintext
Memory loc      Data
0x00            0000000000000000000000000000000000000000000000000000000000000020 // The location of the "Hello, world!" data
0x20            000000000000000000000000000000000000000000000000000000000000000d // The length of "Hello, world!" in bytes
0x40            48656c6c6f2c20776f726c642100000000000000000000000000000000000000 // Value "Hello, world!"
```

## Implementation

The following `MAIN` macro steps through this encoding in a clear way.

```javascript

#define macro MAIN() = takes(0) returns(0) {
    // store dynamic offset of 0x20 at 0x00
    0x20                                     // [0x20]
    0x00                                     // [0x00, 0x20]
    mstore                                   // []

    // store string length of 0x0d at 0x20
    0x0d                                     // [0x0d]
    0x20                                     // [0x20, 0x0d]
    mstore                                   // []

    // store bytes for "Hello, world!" at 0x40
    __RIGHTPAD(0x48656c6c6f2c20776f726c6421) // ["Hello, world!"]
    0x40                                     // [0x40, "Hello, world!"]
    mstore                                   // []

    // return full 96 byte value
    0x60                                     // [0x60]
    0x00                                     // [0x00, 0x60]
    return                                   // []
}

```

Have a look how memory is set and what is returned interactively with [evm.codes playground](https://www.evm.codes/playground?unit=Wei&codeType=Mnemonic&code='v20~0z~0d~2zws32t48656c6c6f2c20776f726c6421yyyyyyu~4z~60~uwRETURN'~wvz0wMSTOREwyuuuw%5Cnvs1tu00t%200xsPUSH%01stuvwyz~_) for this example.

## Advanced topic - The Seaport method of returning strings

Notice in the previous example that 3 adjacent words (32 bytes each) are being stored in memory. The 1st word is the offset and the 2nd word is the length. Both of these are left padded as all values are. The 3rd word is right padded because its bytes. Notice the length and the bytes are adjacent to each other in the memory schema. The length, "0d" is in memory location 0x3F and the first byte of "Hello, world!", 0x48, is stored at 0x40.

If we take the length (`0x0d`) and the bytes (`0x48656c6c6f2c20776f726c6421`), and concatenate them, we would get: `0x0d48656c6c6f2c20776f726c6421` which becomes left padded value of:

```plaintext
0x000000000000000000000000000000000000000d48656c6c6f2c20776f726c6421
```

Now, instead of starting the second word at `0x20`, if we offset that by 13 bytes (starting at `0x2d` instead of `0x20`) then it lines up so that the `0d` falls in the right most (lowest) bits of the second word (location 0x3F) and the remaining bytes start immediately in the first leftmost (highest) byte of the third word (location 0x40).

This is a common technique in Huff and was also made popular by [Seaport's \_name() function](https://github.com/ProjectOpenSea/seaport/blob/fb1c3bf4c25a32ae90f776652a8b2b07d5df52cf/contracts/Seaport.sol#L95-L108).

Here is a diagram illustrating the "Seaport" method using the string "TKN":

![The "Seaport" method](../images/Seaport.png)
