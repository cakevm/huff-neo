# Huff Neo CLI

While most of the time you will be compiling your Huff contracts in a foundry
project using the [foundry-huff-neo](https://github.com/cakevm/foundry-huff-neo)
library, the [compiler](https://github.com/cakevm/huff-neo)'s CLI offers some additional configuration options as well as some useful utilities.

## Options

```plaintext
Huff Language Compiler built in Pure Rust.

Usage: hnc [OPTIONS] [PATH] [COMMAND]

Commands:
  test  Test subcommand
  help  Print this message or the help of the given subcommand(s)

Arguments:
  [PATH]  The contract(s) to compile

Options:
  -s, --source-path <SOURCE>              The contracts source path [default: ./contracts]
  -o, --output <OUTPUT>                   The output file path
  -d, --output-directory <OUTPUTDIR>      The output directory [default: ./artifacts]
  -i, --inputs <INPUTS>...                The input constructor arguments
  -n, --interactive                       Interactively input the constructor args
  -a, --artifacts                         Whether to generate artifacts or not
      --relax-jumps                       Apply branch relaxation to minimize deployment gas
  -g, --interface [<INTERFACE>...]        Generate solidity interface for a Huff artifact
  -b, --bytecode                          Generate and log bytecode
  -r, --bin-runtime                       Generate and log runtime bytecode
  -p, --print                             Prints out to the terminal
  -v, --verbose                           Verbose output
  -l, --label-indices                     Prints out the jump label PC indices for the specified contract
      --format <FORMAT>                   Output format for the label indices [default: table] [possible values: table, json]
  -c, --constants <CONSTANTS>...          Override / set constants for the compilation environment
  -m, --alt-main <ALTERNATIVE_MAIN>       Compile a specific macro
  -t, --alt-constructor <ALT_CONSTRUCTOR> Compile a specific constructor macro
  -e, --evm-version <EVM_VERSION>         Set the EVM version [default: osaka]
      --flattened-source                  Output the flattened source code with all dependencies resolved
      --no-size-limit                     Skip the contract and initcode size limit checks (EIP-170/EIP-3860, raised by EIP-7954 in Amsterdam)
  -V, --version                           Print version
  -h, --help                              Print help
```

### `-a` Artifacts

Passing the `-a` flag will generate `Artifact` JSON file(s) in the `./artifacts`
directory or wherever the `-d` flag designates. The `Artifact` JSON contains
the following information:
* File
  * Path
  * Source
  * Dependencies
* Bytecode
* Runtime Bytecode
* Contract ABI

Example:
```shell
hnc ./src/ERC20.huff -a
```

### `-b` Bytecode

Passing the `-b` flag will tell the compiler to log the bytecode generated during
the compilation process to the console.

Example:
```shell
hnc ./src/ERC20.huff -b
```

### `-c` Constants

Arguments: `[CONSTANTS]`

Passing the `-c` flag allows you to override and set constants for the current compilation environment. Values can be:
- Hex literals in `0x` format (up to 32 bytes)
- `true` (equivalent to `0x01`)
- `false` (equivalent to `0x00`)

Example:
```shell
hnc ./Test.huff -c MY_CONST=0x01 MY_OTHER_CONST=0xa57b DEBUG=true
```

### `-d` Output directory

Arguments: `<OUTPUT_DIR>`, Default: `./artifacts`

Passing the `-d` flag allows you to designate the directory that the `Artifact`
JSON file will be exported to.

Example:
```shell
hnc ./src/ERC20.huff -d ./my_artifacts
```

### `-e` EVM Version

Arguments: `<EVM_VERSION>`, Default: `osaka`

Passing the `-e` flag allows you to set the target EVM version for compilation.
This determines which opcodes are available. Supported versions:
- `paris` - PREVRANDAO
- `shanghai` - PUSH0
- `cancun` - TLOAD, TSTORE, MCOPY, BLOBHASH, BLOBBASEFEE
- `prague` - No new opcodes
- `osaka` - CLZ (default)
- `amsterdam` - SLOTNUM, DUPN, SWAPN, EXCHANGE, and the larger contract size limits of EIP-7954

The EVM version also selects the contract size limits (see [`--no-size-limit`](#--no-size-limit)).
When running tests, an explicit `-e` also executes them with the rules and gas schedule of that
hardfork, instead of the one from the Foundry config.

Example:
```shell
hnc ./src/ERC20.huff -e cancun
hnc ./src/Deep.huff -e amsterdam test
```

### `--flattened-source`

Passing the `--flattened-source` flag outputs the flattened source code with all
dependencies and includes resolved into a single output.

Example:
```shell
hnc ./src/ERC20.huff --flattened-source
```

### `--no-size-limit`

Passing the `--no-size-limit` flag bypasses the contract size limit checks.
By default, compilation fails if the bytecode exceeds the limits of the target EVM version:

| EVM version            | Runtime bytecode (EIP-170 / EIP-7954) | Deployment bytecode (EIP-3860 / EIP-7954) |
|------------------------|---------------------------------------|-------------------------------------------|
| up to `osaka`          | 24,576 bytes                          | 49,152 bytes                              |
| `amsterdam` and later  | 65,536 bytes                          | 131,072 bytes                             |

The deployment bytecode includes the constructor, the runtime code, and encoded constructor
arguments.

Example:
```shell
hnc ./src/LargeContract.huff -b --no-size-limit
```

### `-g` Interface

Passing the `-g` flag will generate a Solidity interface for the Huff contract
provided. This interface is generated based off of the function and event
definitions within the contract.

The solidity file will always be named `I<HUFF_FILE_NAME>.sol`, and it will be
saved in the same directory as the Huff contract itself.

Example:
```shell
hnc ./src/ERC20.huff -g
```

### `-i` Inputs

Arguments: `[CONSTRUCTOR_ARGS]`

Passing the `-i` flag allows you to set the constructor arguments for the
contract that is being compiled. All inputs should be separated by a comma.
If you'd like to input the constructor arguments interactively instead,
use the `-n` flag.

Example (assuming `ERC20.huff`'s constructor accepts a String and a uint):
```shell
hnc ./src/ERC20.huff -i "TestToken", 18
```

### `-l` Label Indices

Passing the `-l` flag prints out the jump label PC (program counter) indices
for the specified contract. This is useful for debugging and understanding
where labels resolve to in the final bytecode.

Example:
```shell
hnc ./src/ERC20.huff -l
```

Add `--format json` to get machine-readable output for scripts. Each entry
contains the label name, its PC offset as a decimal integer, and the macro
scope path (just `["MAIN"]` for labels defined directly in `MAIN`):

```shell
hnc ./src/ERC20.huff -l --format json
```

```json
[
  {
    "label": "inner",
    "offset": 9,
    "scope": ["MAIN", "INNER_5"]
  }
]
```

### `-m` Alternative Main

Arguments: `<ALTERNATIVE_MAIN>`

Passing the `-m` flag allows you to compile a specific macro as the main
entry point instead of the default `MAIN` macro.

Example:
```shell
hnc ./src/Contract.huff -m MY_CUSTOM_MAIN
```

### `-n` Interactive Inputs

Passing the `-n` flag allows you to input constructor arguments
interactively through the CLI rather than via the `-i` flag.

Example:
```shell
hnc ./src/ERC20.huff -n
```

### `-o` Output

Arguments: `<FILE_PATH>`

Passing the `-o` flag allows you to export the artifact to a specific file
rather than a folder.

Example:
```shell
hnc ./src/ERC20.huff -o ./artifact.json
```

### `-p` Print

Passing the `-p` flag prints the compilation output to the terminal.

Example:
```shell
hnc ./src/ERC20.huff -p
```

### `--relax-jumps`

Passing the `--relax-jumps` flag applies branch relaxation to minimize deployment
gas costs. When enabled, all pushes for jumps will be minimized to PUSH1 where
possible. This can reduce deployment gas costs but has no effect on runtime gas
costs. Only applies to label references used in JUMPI and JUMP opcodes. Independent of this
flag, jumps to targets beyond 64 KiB (possible in constructors from Amsterdam) use PUSH3.

Example:
```shell
hnc ./src/ERC20.huff --relax-jumps -b
```

### `-s` Source Path

Arguments: `<CONTRACTS_FOLDER>`, Default: `./contracts`

Passing the `-s` flag allows you to change the directory that the compiler scans
for Huff contracts.

Example:
```shell
hnc -s ./src/
```

### `-t` Alternative Constructor

Arguments: `<ALTERNATIVE_CONSTRUCTOR>`

Passing the `-t` flag allows you to compile a specific macro as the constructor
entry point instead of the default `CONSTRUCTOR` macro.

Example:
```shell
hnc ./src/Contract.huff -t MY_CUSTOM_CONSTRUCTOR
```

### `-r` Runtime Bytecode

Passing the `-r` flag will tell the compiler to print the runtime bytecode
of the compiled contract.

### `-v` Verbose Output

Passing the `-v` flag will tell the compiler to print verbose output during
the compilation process. This output can be useful for debugging contract
as well as compiler errors.

Example:
```shell
hnc ./src/ERC20.huff -v
```

## Subcommands

### `test`

- Format: `hnc ./path/to/Contract.huff test [-f <list|table|json>] [-m <TEST_NAME>]`

The test subcommand is the entry point to running tests within a Huff contract.

#### Optional Flags
* `-f` or `--format`: Formats the test report as a list, table, or JSON.
* `-m` or `--match`: Runs a specific test with the name passed to this flag.
