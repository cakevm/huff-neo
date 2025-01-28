<!-- Keep a Changelog guide -> https://keepachangelog.com -->

# Huff Neo Compiler changelog

## [Unreleased]
- Allow to use `--debug` for reverting contracts.

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