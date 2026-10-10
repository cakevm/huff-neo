# Deep Stack Access

`dup1`–`dup16` and `swap1`–`swap16` can only reach the top 17 stack items. The Glamsterdam
upgrade ([EIP-8024](https://eips.ethereum.org/EIPS/eip-8024)) adds three instructions that reach
deeper into the stack. They are available with `-e amsterdam`.

| Instruction        | Opcode | Operands                    | Effect                                                |
|--------------------|--------|-----------------------------|-------------------------------------------------------|
| `dupn <n>`         | `0xe6` | `17 <= n <= 235`            | Duplicates the n-th stack item (like `dup<n>`)        |
| `swapn <n>`        | `0xe7` | `17 <= n <= 235`            | Swaps the top with the (n+1)-th item (like `swap<n>`) |
| `exchange <n> <m>` | `0xe8` | `1 <= n < m`, `n + m <= 30` | Swaps the (n+1)-th and (m+1)-th items                 |

Each instruction costs 3 gas and is two bytes long: the opcode followed by an immediate byte.

## Writing operands

Operands are stack positions written directly after the instruction. The compiler encodes them into
the immediate byte. An operand can be any compile-time value:

```javascript
#define constant DEPTH = 0x14

#define macro DUP_AT(depth) = takes(0) returns(1) {
    dupn <depth>          // macro argument
}

#define macro MAIN() = takes(0) returns(0) {
    // ... 20 items on the stack
    dupn 17               // decimal: like a "dup17", copies the 17th item to the top
    swapn 0x14            // hex: like a "swap20", swaps the top with the 21st item
    exchange 1 2          // swaps the 2nd and 3rd item
    dupn [DEPTH]          // constant
    dupn ([DEPTH] - 1)    // arithmetic in parentheses
    DUP_AT(18)            // macro argument (decimal or hex)
    for(i in 17..20) {
        dupn <i>          // loop variable
    }
}
```

The numbering matches the existing instructions, so `dupn 17` continues where `dup16` ends.
The immediate byte uses a special encoding: some byte values would look like a `JUMPDEST` or a
`PUSH` instruction. Always write the stack position; the compiler never emits these byte values.

Positions outside the valid range are a compile error. Literal positions are checked while
parsing, all others once their value is known:

```plaintext
Error: Invalid stack operand for "Dupn"
"Dupn" takes one stack depth between 17 and 235
  > 3 |     dupn 16
```

For depths up to 16, keep using `dup1`–`dup16` and `swap1`–`swap16`. They are one byte shorter.

## Limitations

`dupn`, `swapn`, and `exchange` cannot be passed as macro arguments, since the argument would not
carry the operands. Pass the stack position instead, or wrap the instruction in a macro:

```javascript
#define macro DUP20() = takes(0) returns(1) {
    dupn 20
}
```
