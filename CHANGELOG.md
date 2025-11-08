<!-- Keep a Changelog guide -> https://keepachangelog.com -->

# Huff Neo Compiler changelog

## Unreleased

## [1.5.5] - 2025-11-08
- Fix `--relax-jumps` not updating label positions always correct during iterative optimization.
  - Fix jump table entries are now correctly updated after each relaxation iteration.
  - Fixes issue where jumps at the PUSH1/PUSH2 boundary (byte 255/256) were not optimized.
  - Use correct PC for `__ASSERT_PC` after jump relaxation.
- Correctly handle decimal literals in for loop bounds (have been interpreted as hex before).
  - Example: `for(i in 0..255)` and `for(i in 0..0xff)` are now equivalent.
- Add `__EMBED_TABLE(TABLE_NAME)` builtin function to embed code tables inline.
  - Embeds table bytes at the current position instead of at the end of bytecode.
  - Each table can only be embedded once to avoid ambiguity.
  - `__tablestart(TABLE)` returns the inline embedding position for embedded tables.
- Add support for `keccak256` opcode as an alias for `sha3` (fixes #145).
    - Both `keccak256` and `sha3` compile to the same EVM opcode (0x20).
- Fix built-in functions not working inside if-statement bodies (fixes #144).
    - Example: `if (true) { __RIGHTPAD(0x1234) }`

## [1.5.4] - 2025-11-07
- Add support for `true` and `false` boolean literals in if conditions and macro arguments.
  - `true` evaluates to `0x01`, `false` evaluates to `0x00`.
  - Example: `if (true) { 0xAA } else { 0xBB }`
- Fix macro argument scoping bug where parameters leaked across macro boundaries (fixes #139).
  - Macros can no longer access parameters from parent scopes unless explicitly passed.
  - Example: If `M1(arg)` calls `M2()` without passing `arg`, then `M2` cannot use `<arg>`.
- Fix nested macro invocation resulted in stack overflow (fixes #140).
  - Example: `M1(M1(M1(<a>)))`

## [1.5.3] - 2025-11-06
- Introducing `--relax-jumps` CLI flag to optimize jump instructions from PUSH2 to PUSH1.
  - Optimizes jump instructions from PUSH2 to PUSH1 when jump targets are 0-255.
  - Reduces deployment gas costs, but will not affect runtime gas costs.
- Removed unimplemented `--optimize` flag.
- Fix parser bug where `for` loops and `if/else` statements after labels were rejected.
- Allow `for`, `if`, and `else` to be used as label names.
- Add parser support for `<arg>` in if/for expressions (fixes #129).
  - Example: `if (<MODE> == 0x01) { ... }`
- Add support for builtin functions as macro arguments (fixes #130).
  - Builtin functions can now be passed as macro arguments: `__FUNC_SIG`, `__EVENT_HASH`, `__BYTES`, `__RIGHTPAD`.
  - Example: `MACRO(__FUNC_SIG(transfer))`
- Reject macros names that are equal to reserved builtin function names (fixes #131).
- Fix stack overflow and argument resolution errors in nested macro invocations with labels (fixes #133).
  - Example: `M2(M3(<arg>))` followed by a label now compiles without errors.
- Add Windows ARM64 (`aarch64-pc-windows-msvc`) binary to release targets.

## [1.5.2] - 2025-11-05
- Add compile-time if/else if/else statements.
  - Example: `if ([MODE] == 0x01) { 0xAA } else if ([MODE] == 0x02) { 0xBB } else { 0xCC }`
- Fix constant substitution failing when referencing builtin functions (fixes #122).
  - Example: `#define constant C1 = __RIGHTPAD(0x)` and `#define constant C2 = [C1]` now works correctly.
  - Applies to all builtin functions: `__FUNC_SIG`, `__EVENT_HASH`, `__RIGHTPAD`, `__LEFTPAD`, `__BYTES`.
- Fix nested macro invocation argument scoping issue (fixes #123).
  - The compiler now properly searches through the entire invocation stack to resolve argument names.

## [1.5.1] - 2025-11-04
- Throw error for circular constant dependencies to prevent infinite loops during constant evaluation.
  - Detects direct cycles (A = B, B = A), indirect cycles (A = B, B = C, C = A), and self-references (A = A + 1).
- Fix `__NOOP` not working as a macro argument (e.g. `MACRO(__NOOP)`, `<m>(__NOOP)`) (fixes #118).
- Fix deadlock when compile-time evaluated constants are passed as macro arguments (fixes #119).
  - The compiler previously hung when passing constants (e.g., `#define constant C2 = [C1]`) as macro arguments.

## [1.5.0] - 2025-11-02
- **Breaking**: Bracket notation `[CONSTANT_NAME]` is now required for referencing constants in arithmetic expressions and for-loop bounds (fixes #115).
  - Example: `#define constant RESULT = [A] + [B]` (was: `A + B`)
  - This follow the Huff language convention for constant values using brackets.
  - Applies to: constant definitions with arithmetic expressions, for-loop bounds (`for(i in [START]..[END])`), and for-loop step values.
  - Migration: Update code from `for(i in START..END)` to `for(i in [START]..[END])` and arithmetic expressions from `A + B` to `[A] + [B]`.

## [1.4.0] - 2025-11-02
- Add compile-time for-loops that expand during compilation.
  - Syntax: `for(variable in start..end) { body }` or `for(variable in start..end step N) { body }`
  - Support for a loop variable with `<variable>`.
    - Example: `for(i in 0..5) { <i> }` expands to `0x00 0x01 0x02 0x03 0x04`
- Add `__NOOP` builtin constant that generates no bytecode (fixes #111).
  - Can be used e.g. for optional macro arguments `MACRO(__NOOP)`.
- Add `__ASSERT_PC(<literal>|<constant>)` builtin function for compile-time bytecode position assertions.
  - Useful for ensuring `JUMPDEST` are at expected offsets.

## [1.3.10] - 2025-11-01
- Fix macro argument scoping to prevent arguments from leaking into nested macros that don't receive them (fixes #108).
- Support the new opcode CLZ (Count Leading Zeros) introduced in the Osaka upgrade.
  - See: https://eips.ethereum.org/EIPS/eip-7939.
- Add new error handling for opcodes that are not available in the selected EVM version.

## [1.3.9] - 2025-10-18
- Add arithmetic support for constants with operators: `+`, `-`, `*`, `/`, `%`, and negation.
  - Example: `#define constant SUM = 0x01 + 0x02 // Results in 0x03`

## [1.3.8] - 2025-10-11
- Fix constant validation to allow numbers in constant names (not first char) (e.g., `VALUE_V1`).
- Update foundry to v1.4.0.

## [1.3.7] - 2025-09-02
- Fix parsing of first-class macro invocations inside label definitions (fixes #103).

## [1.3.6] - 2025-08-30
- Fix parsing of nested first-class macro argument invocations like `<m>(<a>())` (fixes #96).
- Fix infinite loop when compiling nested first-class macro invocations by resolving arguments at invocation time (fixes #98).
- Fix label resolution in sibling scopes when labels are passed through nested macro invocations (fixes #97).

## [1.3.5] - 2025-08-23
- Fix invoking nested first-class macro with parameters (`<arg>()` syntax) (fixes #94).

## [1.3.4] - 2025-08-16
- Fix TypeScript definitions to use `Map<string, T>` types matching runtime behavior with ES2015+ support.

## [1.3.3] - 2025-08-16
- Add `--flattened-source` CLI flag to output the flattened source code (with all includes resolved).
- Add source code display in debugger with proper multi-file support.
- Improve TypeScript definitions for WASM/JS package with proper types instead of `any`.

## [1.3.2] - 2025-08-14
- Standardize span convention to use exclusive end positions (following Rust Range convention).
  - All spans now use `start..end` where `end` is exclusive (points after the last character).
- Fix source mapping to correctly handle nested macro expansions.
  - Spans now point to actual opcodes in macro definitions, not invocations.

## [1.3.1] - 2025-08-14
- Add constructor and runtime source maps in Artifact structure.
- Remove UUID field from FileSource struct for better WASM compatibility.
- Fix span end to be inclusive at the end.

## [1.3.0] - 2025-08-12
- Update rust to 1.89 and dependencies to the latest version.
- **Breaking**: Implement proper label scoping to prevent label overwriting in macro invocations. (fixes #82)
  - Each macro invocation now has its own label scope.
  - Duplicate labels within the same scope are properly detected and reported.
  - Label shadowing across different scopes is supported.
  - Fixes issue where multiple invocations of the same macro would cause all jumps to target the last label definition.
- Fix nested macro invocation with labels causing panic. (fixes #77)
  - Correctly handle bytecode offset tracking when macros are expanded as arguments.
- Handle circular macro recursion and prevent stack overflow. (fixes #75)
  - Add detection for circular macro invocations with clear error messages.
- Implement first-class macro arguments. (fixes #41)
  - Macros can now be passed as arguments to other macros.
  - New syntax: `<arg>()` to invoke a macro passed as an argument.

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