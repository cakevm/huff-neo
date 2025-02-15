## Compiling Contracts with the Huff Compiler

_NOTE: Installing the Huff Compiler is a prerequisite for compiling contracts. See [installing](getting-started.md) to install `hnc`._

Below we outline the few steps it takes to compile a Huff contract.

**1. Create a file called `addTwo.huff` and enter the following:**

```javascript,norun,noplayground,ignore
{{ #include ../tutorial/examples/addTwo.huff }}
```

**2. Use `hnc` to compile the contract and output bytecode:**

   ```shell
   hnc addTwo.huff --bytecode
   ```

   This will output something similar to:

   ```plaintext
   600c8060093d393df35f35602035015f5260205ff3
   ```


You can find an in-depth explanation of this contract in [The Basics](../tutorial/the-basics.md) tutorial.
