<!-- Keep a Changelog guide -> https://keepachangelog.com -->

# Huff Neo Compiler changelog

## [Unreleased]

## [1.2.0] - 2025-07-16
- Fix table instance propagation in nested macro calls (fixes #76).
- **Breaking**: Fix resolve nested macro argument evaluation order (fixes #80).
  -	The evaluation order of macro arguments is now left-to-right and no longer evaluates the most deeply nested macro first.
- Update dependencies to the latest version.

## [1.1.14] - 2025-06-17
- Fix using correct chain id for transactions in test runner.

## [1.1.13] - 2025-06-14
- Update dependencies to the latest version.
- Fix nested macro argument resolution (fixes #60, #48).
- Fix label resolution for macros passed as arguments (fixes #62).
- Fix macro argument scoping and execution order (fixes #50).

## [1.1.12] - 2025-05-30
- Clean release after CI fixes.

## [1.1.11] - 2025-05-30
- Update rust to 1.87.

## [1.1.10] - 2025-05-30
- Remove `AUTH` and `AUTHCALL` opcodes since EIP-3074 was withdrawn from Pectra.
  - See: https://github.com/ethereum/EIPs/pull/9771

## [1.1.9] - 2025-05-29
- Hotfix to get `huff-js` wasm compiling.

## [1.1.8] - 2025-05-29
- Convert context to a list of contexts to allow for nested contexts in lexing.
- Make Prague the default EVM version.
- Update `revm` and `foundry` to the latest version.
- Add `AUTH` and `AUTHCALL` opcodes to the code table.
- Remove EOF opcodes from the code table.

## [1.1.7] - 2025-03-30
- Throw error if the argument count for a macro call is not equal to the macro definition (Fixes: #49).
- Fix error where an opcode could not be handled as second argument in a macro call (Fixes: #50).
- Release `huff-neo-js` to npm.

## [1.1.6] - 2025-03-29
- Throw an error if an argument (e.g. `<arg>`) is used but not defined (Fixes: #46).
- **Breaking**: Disallow the use of the same name for a macro, test, or fn (Fixes: #53).
- Throw an error if an included file is not found in a nested include (Fixes: #51).
- Fix bug where 0x00 as argument is not rendered as PUSH0 (Fixes: #52).

## [1.1.5] - 2025-03-19
- Support nesting of macro calls e.g. `MACRO1(MACRO2(0x1, 0x2), 0x3)`. (See: #40)
  - Thank you very much, @Mouradif, for the contribution!

## [1.1.4] - 2025-03-17
- Update dependencies to the latest version.

## [1.1.3] - 2025-02-21
- Add EOF opcodes.
- Add EVM version entry for `Prague` and `Osaka`.
- Fix duplicate code tables in bytecode #37.

## [1.1.2] - 2025-02-06
- Update to the latest `revm` and `foundry` versions.
- Remove parameters without effect from `test` that have been added from Foundry.

## [1.1.1] - 2025-02-02
- Update dependencies to the latest version.

## [1.1.0] - 2025-02-02
- Support for constants in code tables.
- Use correct code line for error messages in code tables for built-ins.
- Switch to Foundry's print function for trace logs.
- Allow using `--target-address` to specify the contract address during a test.
- Disable base fee check for tests.
- Allow uneven bytes for code tables e.g. `0x1` is valid and results in `0x01` bytecode.
- Introduce new lexer token type `Bytes`, as not all hex should be parsed to bytes32.
  - For constants in code tables, the bytes are copied as defined with e.g. leading zeros.
  - For all other cases, leading zeros are removed and the smallest push operation is used.
- New built-in function `__LEFTPAD` to a pad a hex input or a function result in a code table to the left in 32 bytes.
  - The function can only be used in a code table.
  - Example: `__LEFTPAD(0x123)` -> `0000000000000000000000000000000000000000000000000000000000000123`
- Allow to pass a constant as a parameter to `__RIGHTPAD` or `__LEFTPAD`.
  - Example: `__RIGHTPAD([CONST])`
- Allow to use built-in functions in constant assignment.
  - Example: `#define constant FUNC_TEST = __FUNC_SIG("test(uint256)")`
  - This solves the issue to define functions with the same name but different arguments.

## [1.0.10] - 2025-01-31
- Add windows binary to the release.
- Add linux binary build with `musl` to the release.
  - This can be helpful for e.g. outdated glibc version in a Docker container.
- Revert static linking for the linux binary.

## [1.0.9] - 2025-01-31
- Use static linking for the linux binary.
  - This resolves issues when `hnc` is on a linux with a different glibc version.

## [1.0.8] - 2025-01-29
- Support built-in function calls in code table (`#define table`) body.
  - Supported are `__BYTES`, `__FUNC_SIG`, and `__RIGHTPAD`.

## [1.0.7] - 2025-01-29
- Add built-in macros for converting a string to bytes and push it to the stack.
  - `__BYTES("hello")` -> `PUSH5 0x68656c6c6f`
  - This can also be used here: `__RIGHTPAD(__BYTES("hello"))`.

## [1.0.6] - 2025-01-28
- Allow to use `--debug` for reverting contracts.
- Refactored parsing for ABI and build-in argument.
  - This is the first step to push more steps into the lexing phase. 
- Extend parsing to allow `__FUNC_SIG` inside of `__RIGHTPAD`.
  - Example: `__RIGHTPAD(__FUNC_SIG('transfer(address,uint256)'))`.

## [1.0.5] - 2025-01-27
- Use foundry's debugger for `--debug` flag.

## [1.0.4] - 2025-01-27
- Rewrite the Huff test module to use `anvil` and `forge` features from `foundry` to fork the mainnet.
  - This is experimental, and there will be some breaking changes.
- Allow printing logs with `-vvv` and call traces for tests with `-vvvv`.
- Add `forge` EVM feature flags for testing.
  - **Known issues**: The CLI reports more features than are actually available.
- **Breaking**: Remove the Huff cheat code for logging.
  - The `foundry` `console.log` should now be used.
- **Breaking**: Remove logs from the test JSON output for now.
- Fix error reporting for invalid literals during lexing in the test command.
- Add `Cancun` as an EVM version and make it the default.
- **Breaking**: The `-f` flag for formatting the output is now replaced with the fork URL. Please use `--format` instead.
- Tokio is now used for the test runner to be able to use the db backend from `foundry`.

## [1.0.3] - 2025-01-12
- Remove huff-examples submodule
- Fix invalid error mapping for import with unmatched jump labels
- Print unmatched jump labels

## [1.0.2] - 2025-01-11
- Use latest stable Rust version 1.84
- Report error for invalid hex literals `0x0x`
- Improve lexer performance by 1-2% by removing unnecessary cloning
- Allow to use same name for test and macro or fn

## [1.0.1] - 2025-01-10
- Validate that a constant hex literal is not longer than 32 bytes
- New flatten source algo to determine spans
- Fix all errors related to invalid source code mapping
- Limit label duplicate check to containing macro and file

## [1.0.0] - 2025-01-09
- First stable release of `huff-neo`
- Compiler binary is now `hnc` instead of `huffc`
- Update all dependencies to the latest version
- Restructure to a modern crate structure
- Replace `ethers` with `alloy`
- Remove spinner animation [#4](https://github.com/cakevm/huff-neo/pull/4)
- Fix relative position error for [#6](https://github.com/cakevm/huff-neo/pull/6)
  - Currently, this still fails for nested imports
- Replace `tiny-keccak` with the alloy version
- Add error for duplicate labels in the same macro [#7](https://github.com/cakevm/huff-neo/pull/7)
- Fix recursive error for empty import e.g. `#include ""`
- Fix error for nested imports with reoccurring imports

## [0.0.4] - 2025-01-09
- Improve error handling

## [0.0.3] - 2025-01-09
- Initial release of `huff-neo`