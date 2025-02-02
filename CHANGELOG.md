<!-- Keep a Changelog guide -> https://keepachangelog.com -->

# Huff Neo Compiler changelog

## [Unreleased]

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