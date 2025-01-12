# Huff Neo Compiler

The `hnc` CLI (short for Huff Neo Compiler) allows for compiling Huff contracts to EVM bytecode.

## Usage

To run `hnc` from the command line, you can simply run:

```bash
hnc --help
```

By default, hnc will attempt to compile all contracts in the `contracts` directory. If there is no `contracts` directory present, the following will spit out an error like so:

```bash,color=red
~ hnc

Error: Invalid File Directory ./contracts

```

#### Usage

To run `hnc` against one of the resource test files, the path may simply be passed to `hnc`.

For example, to compile huff-example's [ERC20.huff](../../resources/erc20/ERC20.huff) contract, run:

```bash
hnc --bytecode ./resources/erc20/ERC20.huff
```

_NOTE: The `--bytecode` flag will output the full deploy bytecode._

`hnc` also supports tracing using the [`tracing`](https://docs.rs/tracing/0.1.29/tracing/) crate. To produce a verbose output using tracing, append the `--verbose` or `-v` flag like so:

```bash
hnc --verbose --bytecode ./resources/erc20/ERC20.huff
```

#### Specifying Artifact Outputs

**By default**, `hnc` will export json build artifacts to a `./artifacts` directory. This can be overidden using the `--output-directory` flag or shorthand `-d` flag and specifying a string following. For example:

```bash
hnc -d ./output ./resources/erc20/ERC20.huff
```

_NOTE: The huff cli will gracefully remove double and single quotes, so the following will also compile:_

```bash
hnc -d "./output" './resources/erc20/ERC20.huff'
```

If a specific contract is specified for compiling (ie not a directory), a single `json` file may be specified as an output location for the contract artifact like so:

```bash
hnc -o ./artifact.json ./resources/erc20/ERC20.huff
```

**NOTE**: The following will _not_ compile since multiple artifacts cannot be output to the same artifact json file.

```bash
hnc -o ./artifact.json ./contracts/
```

#### Entering Constructor Arguments

`hnc` supports passing in constructor arguments to the contract. This is done by passing in the `--interactive` (shorthand: `-n`) flag or passing the `--inputs` (shorthand: `-i`) flag.

and passing in the arguments as a comma separated list.

For example, to compile a contract (let's call it `example.huff`) with the following constructor definition:

```huff
#define macro CONSTRUCTOR(uint256, address) = takes(0) returns (0) {
    0x04 calldataload
    0x00 sstore
    0x24 calldataload
    0x01 sstore
}
```

You can enter the arguments `(100, 0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef)` interactively by passing in the `-n` or `--interactive` flag like so:

```bash
$ hnc -b -n ./contracts/example.huff
[INTERACTIVE] Constructor Arguments for Contract: "./contracts/example.huff"
[INTERACTIVE] Enter a uint256 for constructor param: 100
[INTERACTIVE] Enter a address for constructor param: 0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef

335f.....f30000000000000000000000000000000000000000000000000000000000000064000000000000000000000000deadbeefdeadbeefdeadbeefdeadbeefdeadbeef
```

Alternatively, you can enter the arguments as a comma separated list by using the `-i` or `--inputs` flag like so:

```bash
$ hnc -b -i 100, 0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef ./contracts/example.huff
335f0.....f30000000000000000000000000000000000000000000000000000000000000064000000000000000000000000deadbeefdeadbeefdeadbeefdeadbeefdeadbeef
```

#### Other Options

- `-v` or `--verbose`: Outputs detailed logs to the terminal using the [tracing](https://crates.io/crates/tracing) crate.
- `-V` or `--version`: Prints the version of `hnc`.
- `-z` or `--optimize`: Optimizes the contract compilation - a work in progress.
- `-g` or `--interface`: Generates a solidity interface for the contract.

## Building hnc from source

To run `hnc` from the command line, you can use the following command:

```bash
cargo run --bin hnc
```

To pass arguments into the `hnc` binary, simply pass them in after a `--` flag. For example, to get the `hnc` version (a `-V` flag), you can run:

```bash
cargo run --bin hnc -- -V
```

All commands specified in [Usage](#usage) are also available from source by passing them in after the `--` flag.
