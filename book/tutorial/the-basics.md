# The Basics

## Installation

Before you get started writing Huff you will have to install the compiler. Head over to [Getting Started](../get-started/getting-started.md) and follow the steps to get it installed.
Once complete - come back here!!

## What you are going to learn?

Unlike other programming languages, creating a Huff contract that returns "Hello, world!" is quite advanced! To keep things simple we are going to learn how to create a Huff contract that adds two numbers (then we will dive into "Hello, world!").
Open up your editor and create a file called `addTwo.huff`. Lets jump in.

## Add Two

### ABI declaration

First things first. If you're coming from a higher level language like Solidity or Vyper you will be familiar with defining "external" or "public" functions. These allow you to interact with a contract externally by generating an ABI (Application Binary Interface). This describes a contract's entry points to external tools (We will dive more into this later). In this aspect Huff is exactly the same, you can declare functions that will appear in the abi at the top of the file.

```Huff
#define function addTwo(uint256, uint256) view returns(uint256)
```

Go ahead and paste the above example at the top of `addTwo.huff`. This declares a function that takes two `uint256` inputs and returns a single `uint256`.

### The Main Macro

The next thing we are going to create is the `MAIN macro`. This serves a single entry point for Huff contracts. All calls to a contract (regardless of what function they are calling) will start from `MAIN`! In this example we will define a `MAIN` function that will read two `uint256`'s from calldata and return their result.

```Huff
#define macro MAIN() = takes(0) returns(0) {
    0x00 calldataload     // [number1] // load first 32 bytes onto the stack - number 1
    0x20 calldataload     // [number2] // load second 32 bytes onto the stack - number 2
    add                   // [number1+number2] // add number 1 and 2 and put the result onto the stack

    0x00 mstore           // place [number1 + number2] in memory
    0x20 0x00 return      // return the result
}
```

Looking at the above snippet may be intimidating at first, but bear with us.

You'll notice that the MAIN directive is annotated with `takes(0) returns(0)`. As the EVM is a stack based virtual machine (see: [Understanding the EVM](https://docs.huff.sh/tutorial/evm-basics/)), all macro declarations are annotated with the number of items they will `take` from the stack and the amount they will `return` upon completion. When entering the contract the stack will be empty. Upon completion we will not be leaving anything on the stack; therefore, takes and returns will both be 0.

Go ahead and copy the above macro into your `addTwo.huff` file. Run `hnc addTwo.huff --bytecode`.

Congratulations you've just compiled your first contract!

The bytecode output of the compiler will echo the following into the console `600f8060093d393df36000356020350160005260206000f3`.

When you deploy this contract code it will have the runtime bytecode of the main macro we just created! In the above snippet you will find it after the first `f3` (the preceding bytecode is boiler plate constructor logic.)
That leaves us with this: `6000356020350160005260206000f3`
Below, this example dissembles what you have just created!

```plaintext
 BYTECODE          MNEMONIC         STACK                 ACTION
 60 00          // PUSH1 0x00       // [0x00]
 35             // CALLDATALOAD     // [number1]          Store the first 32 bytes on the stack
 60 20          // PUSH1 0x20       // [0x20, number1]
 35             // CALLDATALOAD     // [number2, number1] Store the second 32 bytes on the stack
 01             // ADD              // [number2+number1]  Take two stack inputs and add the result
 60 00          // PUSH1 0x00       // [0x0, (n2+n1)]
 52             // MSTORE           // []                 Store (n2+n1) in the first 32 bytes of memory
 60 20          // PUSH1 0x20       // [0x20]
 60 00          // PUSH1 0x00       // [0x00, 0x20]
 f3             // RETURN           // []                 Return the first 32 bytes of memory
```

If you want to step through the execution yourself you can check out this snippet interactively in [evm.codes](https://www.evm.codes/playground?unit=Wei&codeType=Bytecode&code='~3560203501~526020~f3'~6000%01~_) (pass in the calldata `0x00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002` and click RUN to get started). This calldata is the numbers 1 and 2, both padded to 32 bytes. After running this snippet, you should end up with a return value of `0000000000000000000000000000000000000000000000000000000000000003`. Which is expected! `addTwo.huff` successfully added the numbers 1 and 2, returning 3! If you are new to working with assembly, I strongly suggest you do this as visualizing the individual instructions helps tremendously with learning.

In the next section we will walk through your contract's execution given that you provide the calldata for 2 + 3. Encoded into uint256's (32 bytes) the number 2 would become `0000000000000000000000000000000000000000000000000000000000000002` and the number 3 would become `0000000000000000000000000000000000000000000000000000000000000003`.

This is illustrated in the table below:
| Type | Value | As calldata |
| ----------- | ----------- | ----------- |
| uint256 | 2 | 0000000000000000000000000000000000000000000000000000000000000002 |
| uint256 | 3 | 0000000000000000000000000000000000000000000000000000000000000003 |

By putting the two together, we will send the following calldata to the contract.

```plaintext
0x00000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003
```

### Execution Walk Through

**_Line 1:_** `0x00 calldataload`

This line reads the first 32 bytes of calldata onto the stack. The `calldataload` opcode takes a calldata offset from the stack as its input and returns 32bytes from that offset onto the stack.

_Stack after operation:_ `[2]`

---

**_Line 2:_** `0x20 calldataload`

Similarly, the second line reads the second 32 bytes of our calldata. By pushing the hex number `0x20` (32) onto the triggering `calldataload`.

_Stack after operation:_ `[3,2]`

---

**_Line 3:_** `add`

The third line of ours calls the add opcode. This will take the top two items from the stack as inputs and return the sum of those two numbers. For the inputs `[3,2]` the result is `[5]`

_Stack after operation_ `[5]`

---

**_Lines 4 and 5_**
The remainder of the contract is concerned with returning the result. EVM contracts can only return values that have been stored within the current executions memory frame. This is as the return opcode takes two values as inputs. The offset of memory to start returning from, and the length of memory to return.
In this case the `return` opcode will consume `[0x00, 0x20]`. Or 32 bytes in memory starting from byte 0.

This explains what `0x00 mstore` is there for. `mstore` takes two items from the stack, `[location_in_memory, value]`. In our case we have `[0x00, 0x5]`, this stores the value 5 into memory.

---

### Interacting with this contract externally

As mentioned before, EVM contracts use an ABI to determine which function should be called. Currently, people interacting with addTwo's execution is linear, allowing only one functionality. Most contracts will want to have more than one function. In order to accommodate for this we will have to do a little bit of restructuring.

The contract ABI specification dictates that contract calls will select which function they want to call by prepending a 4 byte (function selector) to their calls. The 4 bytes are sliced from the start of the keccak of the function's abi definition. For example, `addTwo(uint256,uint256)`'s function selector will become `0x0f52d66e` (You can confirm this by using a command line tool such as [`cast`](https://book.getfoundry.sh/cast/)'s `sig` command, or online sites such as [keccak256 online](https://emn178.github.io/online-tools/keccak_256.html)). If you are curious as to what these look like you can find a registry of common 4byte function selectors in the [4 byte directory](https://www.4byte.directory/).

Calculating the function selector each time can be tedious. To make life easy, huff has an included builtin `__FUNC_SIG()`. If a function interface is declared within the file's current scope, it's function selector will be calculated and inlined for you. You can view more information about huff's builtin functions [here](../huff-language/builtin-functions.md).

#### Modifying our contract to accept external function calls

To accept external calls for multiple functions we will have to extract our `addTwo` logic into another macro. Then convert our `MAIN` macro into a function dispatcher.

```Huff
#define function addTwo(uint256,uint256) view returns(uint256)

#define macro MAIN() = takes(0) returns(0) {

    // Get the function selector
    0x00
    calldataload
    0xE0
    shr

    // Jump to the implementation of the ADD_TWO function if the calldata matches the function selector
    __FUNC_SIG(addTwo) eq addTwo jumpi

    addTwo:
        ADD_TWO()
}

#define macro ADD_TWO() = takes(0) returns(0) {
    0x04 calldataload     // load first 32 bytes onto the stack - number 1
    0x24 calldataload     // load second 32 bytes onto the stack - number 2
    add                   // add number 1 and 2 and put the result onto the stack

    0x00 mstore           // place the result in memory
    0x20 0x00 return      // return the result
}
```

The first modifications we make will be within the ADD_TWO macro. On lines 1 and 2 we will shift the calldata offset by 4 bytes for both numbers, this is due to the 4 byte function selector that will be prepended to the calldata value.

Our `MAIN` macro has changed drastically.
The first 4 lines are concerned with isolating the function selector from the calldata.

1. `0x00` pushed `[0]` onto the stack
2. `calldataload` takes `[0]` as input and pushes the first 32 bytes of calldata onto the stack
3. `0xE0` pushes `[224]` onto the stack. This magic number represents 256 bits - 32 bits (28 bytes).
4. When followed by the shr this will shift out calldata by 28 bytes and place the function selector onto the stack.

The following lines will match the function selector on the stack and then jump to the code location where that code is. Huff handles generating all jump logic for you.

Under the hood the ADD_TWO() macro bytecode will be inlined of ADD_TWO() in the main macro.

Now you should be able to use libraries like ethers, or other contracts to call your contract!

We hope this gives you a good understanding of the main concepts and all of the boiler plate you need to get started in Huff!

Next up, we'll dive into more advanced Huff by creating a contract that returns a "Hello, world!" string!
