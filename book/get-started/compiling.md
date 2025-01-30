## Compiling Contracts with the Huff Compiler

_NOTE: Installing the Huff Compiler is a prerequisite for compiling contracts. See [installing](https://docs.huff.sh/get-started/installing/) to install huffc._

Below we outline the few steps it takes to compile a Huff contract.

1. Create a file called `addition.huff` and enter the following:

   ```javascript
   #define function add(uint256, uint256) nonpayable returns (uint256)

   #define macro MAIN() = {
      // Load our numbers from calldata and add them together.
      0x04 calldataload // [number1]
      0x24 calldataload // [number2]
      add               // [number1+number2]

      // Return our new number.
      0x00 mstore // Store our number in memory.
      0x20 0x00 return // Return it.
   }
   ```

2. Use `huffc` to compile the contract and output bytecode:

   ```shell
   huffc addition.huff --bytecode
   ```

   This will output something similar to:

   ```plaintext
   61000f8061000d6000396000f36004356024350160005260206000f3
   ```

You can find an in-depth explanation of this contract in [The Basics](https://docs.huff.sh/tutorial/the-basics) tutorial.
