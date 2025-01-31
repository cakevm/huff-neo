# Getting started

Huff is a low-level programming language designed for developing highly optimized smart contracts that run on the Ethereum Virtual Machine (EVM). Huff does not hide the inner workings of the EVM and instead exposes its programming stack to the developer for manual manipulation.

While EVM experts can use Huff to write highly-efficient smart contracts for use in production, it can also serve as a way for beginners to learn more about the EVM.

If you're looking for an in-depth guide on how to write and understand Huff, check out the [tutorials](../tutorial/overview.md).

## There is `huff-rs` and `huff-neo`?

Yes, there are two versions of the Huff compiler. The original compiler, `huff-rs` is no longer maintained. There is a plan to revive it as [huff2](https://github.com/huff-language/huff2), but since it is a new development from scratch, it is unknown when it will have feature parity with the original compiler. `huff-neo` tries to step-in and continues the development and also tries to evolve the Huff language where necessary.


## History of the Huff Language
The [Aztec Protocol](https://aztec.network/) team originally created Huff to write [Weierstrudel](https://github.com/aztecprotocol/weierstrudel/tree/master/huff_modules), an on-chain elliptical curve arithmetic library that requires incredibly optimized code that neither [Solidity](https://docs.soliditylang.org/) nor [Yul](https://docs.soliditylang.org/en/latest/yul.html) could provide.


