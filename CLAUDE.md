# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Huff Neo is a production-ready compiler for the Huff language - a low-level programming language for developing highly optimized smart contracts that compile to EVM bytecode. This is a maintained fork of the archived huff-rs repository.

## Code Organization Requirements

### Import Statements
- **MUST**: All `use` statements must be placed at the top of the file with the other imports
- **NEVER**: Place `use` statements inside functions or methods
- This ensures consistency, makes dependencies clear at a glance, and improves compilation performance

### Documentation Strings
- **Behavior-focused**: Describe what the code accomplishes, not how it's implemented
- Focus on the "what" and "why" rather than the "how"
- Explain the purpose, inputs, outputs, and any important side effects
- Avoid implementation details that may change over time
- Example: "Resolves a label to its bytecode offset" instead of "Searches the label map and returns the index"

## Build and Development Commands

### Essential Commands
```bash
# Build the compiler
make build              # Debug build
make release           # Release build with optimizations
make maxperf           # Maximum performance build

# Testing
make test              # Run all tests
make test-doc          # Run documentation tests
cargo test --package <crate-name> --test <test-name>  # Run specific test
cargo test <test_function_name>  # Run specific test function

# Code Quality
make fmt               # Format code
make clippy            # Run clippy linter
make taplo             # Format TOML files
make deny-check        # Check dependencies for security/licensing issues

# Pre-release (run before PRs)
make pre-release       # Runs all checks: mdbook test, fmt, taplo, clippy, tests, deny-check

# Documentation
make doc               # Generate Rust documentation
mdbook serve           # Serve the user book locally
```

### Compiler Usage
```bash
# Compile a Huff file
cargo run -- <file.huff> -b              # Show bytecode
cargo run -- <file.huff> -r              # Show runtime bytecode
cargo run -- <file.huff> -a              # Generate artifacts
cargo run -- <file.huff> -o output.json  # Specify output file

# Run tests in Huff files
cargo run -- test <file.huff>            # Run all tests in file
cargo run -- test <file.huff> -m <name>  # Run specific test macro
```

## Architecture Overview

### Crate Structure

The compiler is organized as a Rust workspace with the following key crates:

1. **`bin/hnc`** - CLI interface, handles command-line arguments and orchestrates compilation
2. **`crates/lexer`** - Tokenizes Huff source code into tokens
3. **`crates/parser`** - Parses tokens into an Abstract Syntax Tree (AST)
4. **`crates/codegen`** - Generates EVM bytecode from the AST
5. **`crates/core`** - Core compiler logic, coordinates lexer/parser/codegen
6. **`crates/utils`** - Shared types, error handling, AST definitions
7. **`crates/test-runner`** - Executes Huff test macros using Anvil/REVM
8. **`crates/js`** - WASM bindings for JavaScript/browser usage

### Compilation Pipeline

1. **File Resolution** (`core/src/lib.rs`)
   - Resolves imports and includes
   - Flattens multiple files into a single source

2. **Lexing** (`lexer/src/lib.rs`)
   - Converts source text to tokens
   - Handles context-aware lexing (macro body vs. top-level)
   - Tracks source spans for error reporting

3. **Parsing** (`parser/src/lib.rs`)
   - Builds AST from tokens
   - Main types: `Contract`, `MacroDefinition`, `Statement`
   - Validates syntax and macro signatures

4. **Codegen** (`codegen/src/lib.rs`)
   - Walks the AST to generate bytecode
   - Key methods: `generate_main_bytecode()`, `generate_constructor_bytecode()`
   - Handles label resolution, jump tables, macro expansion

### Key Concepts

#### Label Scoping (Recently Updated)
- Each macro invocation has its own label scope to prevent overwriting
- Labels can be shadowed across different scope depths
- Label resolution order:
  1. Current scope and parent scopes (up the tree)
  2. Child scopes (down the tree) - allows parent to reference nested labels
  3. Sibling scopes (same parent) - fallback for cross-references
- Duplicate labels in siblings only error when cross-referenced
- Scope tracking uses `scope_depth` and `scope_path` (with invocation offsets for uniqueness)

#### Macro System
- **Inline macros**: Code is inserted at call site
- **Function macros** (outlined): Jump to shared code, return via jumpdest
- **Test macros**: Special macros executed by test runner
- Macros support parameters that are substituted during expansion

#### Built-in Functions
- `__FUNC_SIG()`, `__EVENT_HASH()`, `__ERROR()` - ABI encoding helpers
- `__RIGHTPAD()`, `__LEFTPAD()` - Byte padding (code tables only)
- `__BYTES()` - Raw bytes insertion
- `__TABLESIZE()`, `__TABLESTART()` - Jump table utilities
- `__CODESIZE()` - Get size of compiled macro

#### Important Files for Understanding Flow
- `crates/codegen/src/lib.rs` - Main bytecode generation logic
- `crates/codegen/src/irgen/statements.rs` - Statement-level code generation
- `crates/codegen/src/irgen/arg_calls.rs` - Macro argument resolution
- `crates/utils/src/bytecode.rs` - Label indices and scope management
- `crates/utils/src/ast/mod.rs` - AST type definitions

## Testing Approach

### Test Organization
- Unit tests are colocated with source files
- Integration tests in `tests/` directories
- Example contracts in `resources/` for testing compilation

### Key Test Files
- `labels_in_nested_macros.rs` - Label scoping behavior
- `macro_invoc_args.rs` - Macro argument handling
- `erc20.rs`, `erc721.rs` - Full contract compilation tests

## Common Issues and Solutions

### Label Resolution
- Unmatched jump labels indicate undefined labels or scope issues
- Check scope_depth and scope_path in Jump structs for debugging

### Macro Arguments
- Arguments must match macro definition count exactly
- ArgCall resolution bubbles up through parent scopes
- Label arguments search from parent scope perspective

### Error Handling
- Errors include span information for source location
- CodegenError types indicate compilation phase failures
- Use verbose flag (`-v`) for detailed tracing output

## Documentation Maintenance

### Book Updates
When making changes to language features or behavior, always update the corresponding documentation in the `book/` directory:
- `book/huff-language/` - Language feature documentation
- `book/tutorial/` - Tutorial examples that may be affected
- Run `mdbook test` to ensure code examples still compile
- Key files that often need updates:
  - `jump-labels.md` - Label scoping and resolution
  - `macros-and-functions.md` - Macro behavior
  - `builtin-functions.md` - Built-in function changes

## Version and Dependencies

- Rust 1.89+ required
- Uses Alloy for Ethereum types (replacing ethers)
- REVM for EVM execution in tests
- Foundry integration for testing infrastructure