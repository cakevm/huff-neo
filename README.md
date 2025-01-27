<div align="center">

[![CI status](https://github.com/cakevm/huff-neo/actions/workflows/ci.yaml/badge.svg?branch=main)][gh-huff-neo]
[![Telegram Chat][tg-badge]][tg-url]


[gh-huff-neo]: https://github.com/cakevm/huff-neo/actions/workflows/ci.yaml
[tg-badge]: https://img.shields.io/badge/telegram-huff_neo-2CA5E0?style=plastic&logo=telegram
[tg-url]: https://t.me/huff_neo

</div>

# Huff Neo Compiler

The Huff Neo Compiler `hnc` can be used as drop-in replacement for `huffc`. This repository is a hard-fork from [huff-rs](https://github.com/huff-language/huff-rs), hopefully until [huff2](https://github.com/huff-language/huff2) is available. The compiler comes with update dependencies, improved codebase, and several fixes (see [CHANGELOG](https://github.com/cakevm/huff-neo/blob/main/CHANGELOG.md)). This compiler is for all those that require a production-ready compiler right now, as the original repository is archived and not supported anymore. As before, be warned that you are responsible for the contracts you deploy. Find the documentation [here](https://cakevm.github.io/huff-neo-docs).

**Highlights:**
- Fixed line number and lexing/parsing error reporting
- Additional error handling and more details for debugging
- Migration from `ethers` to `alloy` and using latest `revm` version
- Improved stability and updated all dependencies

Feel free to report any issues. Since there is more than just the compiler, the [foundry-huff-neo](https://github.com/cakevm/foundry-huff-neo) has you covered for testing and deployment, and [huff-neo-toolchain](https://github.com/cakevm/huff-neo-toolchain) for your CI/CD needs.

## Why name it `hnc`?

Since the original [huff-rs](https://github.com/huff-language/huff-rs) repository is archived, it makes it more clear that this is something new. You can overwrite the original `huffc` or use it as intended as `hnc` with all the other related tools for Foundry and CI/CD.

## What is Huff?

Huff is a low-level programming language designed for developing highly optimized smart contracts. For a more detailed explanation, see the original repository [huff-rs](https://github.com/huff-language/huff-rs).

The [PR](https://github.com/github-linguist/linguist/pull/6470) to add the Huff Language support on GitHub is still pending.

## Huff Neo Ecosystem

These are the projects that are maintained for Huff Neo:

| Project                                                                          | Scope                    | Support |
|----------------------------------------------------------------------------------|--------------------------|---------|
| [huff-neo](https://github.com/cakevm/huff-neo)                                   | Compiler (This repo)     | ✅       |
| [huff-neo-docs](https://github.com/cakevm/huff-neo-docs)                         | Documentation            | ✅       |
| [foundry-huff-neo](https://github.com/cakevm/foundry-huff-neo)                   | Foundry Plugin           | ✅       |
| [huff-neo-toolchain](https://github.com/cakevm/huff-neo-toolchain)               | GitHub Action (Compiler) | ✅       |
| [huff-neo-tests-action](https://github.com/cakevm/huff-neo-tests-action)         | GitHub Action (Tests)    | ✅       |
| [huff-neo-project-template](https://github.com/cakevm/huff-neo-project-template) | Project Template         | ✅       |
| [huffmate-neo](https://github.com/cakevm/huffmate-neo)                           | Example Contracts        | ✅       |


Supported IDEs:

| Project                                                                | Editor   | Status |
|------------------------------------------------------------------------|----------|--------|
| [intellij-huff-plugin](https://github.com/cakevm/intellij-huff-plugin) | IntelliJ | ✅      |


## How about huff2?

We are very happy that someone picked up the work. In the meantime we still need some compiler to work with. We are trying to keep the original compiler up-to-date with the latest dependencies. Many, many thanks in advance to the [huff2](https://github.com/huff-language/huff2) team!

## Installation

Choose one of the following methods to install the Huff Neo Compiler:

**Option 1:** You can use the installer `hnc-up` to install the latest version of `hnc` from a release or nightly build:
```bash
curl -L https://raw.githubusercontent.com/cakevm/huff-neo/main/hnc-up/install | bash
```

**Option 2:** Build it by cloning the repository and running the following command:
```bash
make release
```

**Option 3:** Install the latest version with cargo:
```bash
cargo install --git https://github.com/cakevm/huff-neo --locked
```

## Modules

- [core](crates/core): The core module to huff-neo-rs. Resolves source file paths, executes compilation, and exports artifacts.
- [cli](bin/hnc): The command line interface for the Huff compiler.
- [js](crates/js): A wasm compatible interface to the Huff compiler for JavaScript bindings.
- [lexer](crates/lexer): Takes in the source of a `.huff` file and generates a vector of `Token`s.
- [parser](crates/parser): Crafts a `Contract` AST from the vector of `Token`s generated by [huff-lexer](crates/lexer).
- [codegen](crates/codegen): EVM Bytecode generation module that accepts an AST generated by [huff-parser](crates/parser).
- [utils](crates/utils): Various utilities and types used by all modules.
- [hnc-up](./hnc-up): Update or revert to a specific huff-neo-rs branch with ease. (Forked from [foundry](https://github.com/foundry-rs/foundry))

## Contributing

Feel free to create any issue or PR. We are always looking for contributors to help us improve the project.

Before submitting a PR, please make sure to run the following commands:
```bash
make pre-release
```

## Safety

> [!CAUTION]
> Please be aware that the resulting bytecode can be unsafe. It is your responsibility to ensure that the contracts are safe and secure. The authors of this project are not responsible for any misuse or loss of funds.

## Acknowledgements

Many thanks to all [huff-rs](https://github.com/huff-language/huff-rs) contributors and to the authors who maintained it for such a long period! Again thanks to the original [Huff Language](https://github.com/huff-language) compiler: [`huffc`](https://github.com/huff-language/huffc). Thanks to [ripc](https://github.com/ibraheemdev/ripc), and big shoutout to [Paradigm](https://github.com/paradigmxyz). Without [Foundry](https://github.com/foundry-rs/foundry) the original implementation would not be possible.

## License
This project is, as the original huff-rs, dual licensed under [Apache 2.0](./LICENSE-APACHE) or [MIT](./LICENSE-MIT) licence.