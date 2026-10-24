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
  -s, --source-path <SOURCE>              A directory whose Huff contracts are compiled when no path is given
  -o, --output <OUTPUT>                   The file the artifact is written to, with `-a`
  -d, --output-directory <OUTPUTDIR>      The directory the artifacts are written to, with `-a` [default: ./artifacts]
  -i, --inputs <INPUTS>...                The constructor arguments, separated by spaces
  -n, --interactive                       Prompt for the constructor arguments, then write the artifacts
  -a, --artifacts                         Whether to generate artifacts or not
      --relax-jumps                       Apply branch relaxation to minimize deployment gas
  -g, --interface [<INTERFACE>...]        Generate a Solidity interface next to each contract, optionally with this name
  -b, --bytecode                          Generate and log bytecode
  -r, --bin-runtime                       Generate and log runtime bytecode
  -v, --verbose                           Verbose output
  -l, --label-indices                     Prints out the jump label PC indices for the specified contract
      --format <FORMAT>                   Output format for the label indices [default: table] [possible values: table, json]
  -c, --constants <CONSTANTS>...          Override / set constants for the compilation environment
  -m, --alt-main <ALTERNATIVE_MAIN>       Compile a specific macro
  -t, --alt-constructor <ALT_CONSTRUCTOR> Compile a specific constructor macro
  -e, --evm-version <EVM_VERSION>         The EVM version to compile for, and to run tests with [default: osaka]
      --no-size-limit                     Skip the contract and initcode size limit checks (EIP-170/EIP-3860, raised by EIP-7954 in Amsterdam)
      --flattened-source                  Output the flattened source code with all dependencies resolved
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

A constant does not need a `#define` to be set this way: the source can reference it and leave the
value to `-c`. Compilation fails if a name passed to `-c` appears nowhere in the source, as such an
override has no effect and is most likely a typo.

### `-d` Output directory

Arguments: `<OUTPUT_DIR>`, Default: `./artifacts`

Passing the `-d` flag together with `-a` designates the directory that the `Artifact`
JSON files are exported to. Without `-a`, no artifact is written.

Example:
```shell
hnc ./src/ERC20.huff -a -d ./my_artifacts
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

An unknown version is rejected and the error lists the supported ones.
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

The Solidity file is named `I<HUFF_FILE_NAME>.sol` and saved in the same directory as the Huff contract.
When a single contract is compiled, `-g` takes an optional name for the interface instead. With several
contracts, the name is ignored.

Example:
```shell
hnc ./src/ERC20.huff -g
hnc ./src/ERC20.huff -g IToken
```

### `-i` Inputs

Arguments: `[CONSTRUCTOR_ARGS]`

Passing the `-i` flag sets the constructor arguments of the contract that is being compiled. The
arguments are separated by spaces and ABI-encoded after the bytecode. The constructor's types come
from a `#define function constructor(...)` in the contract. To enter the arguments interactively
instead, use the `-n` flag.

Example (assuming `ERC20.huff`'s constructor accepts a string and a uint):
```shell
hnc ./src/ERC20.huff -i "TestToken" 18 -b
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

Passing the `-n` flag prompts for each constructor argument instead of taking them from `-i`, appends
them to the bytecode, and writes the artifacts to the `-o` file or the `-d` directory. Add `-b` to also
print the bytecode.

Example:
```shell
hnc ./src/ERC20.huff -n
```

### `-o` Output

Arguments: `<FILE_PATH>`

Passing the `-o` flag together with `-a` exports the artifact to a specific file rather than a
folder. Without `-a`, no artifact is written.

Example:
```shell
hnc ./src/ERC20.huff -a -o ./artifact.json
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

Arguments: `<CONTRACTS_FOLDER>`

Passing the `-s` flag compiles every Huff contract in the given directory when no path is given.
A directory passed as the path does the same.

Example:
```shell
hnc -s ./src/ -b
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
of the compiled contract. It fails for a contract whose `CONSTRUCTOR` returns its own bytecode,
because the deployed runtime is then only known at deployment. Use `-b` for such contracts.

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

- Format: `hnc ./path/to/Contract.huff test [OPTIONS]`

The test subcommand runs the tests of a Huff contract on Foundry's EVM executor, the same engine as
`forge test`. It exits with status `1` when any test fails, so scripts and CI can rely on the exit code.
See [Tests](./huff-language/tests.md) for writing tests.

#### Test options
* `--format <list|table|json>`: Formats the test report. Default: `list`. Other values are rejected.
* `-m` or `--match <NAME>`: Runs only the test with this name.
* `--target-address <ADDRESS>`: Runs each test at this address instead of deploying it, for example to
  replay a deployed contract on a fork.
* `--debug`: Opens a single test in the interactive debugger. Use `-m` to pick the test.
* `--decode-internal`: Shows the calls of Huff functions (`#define fn`) in traces, with their stack arguments and
  return values. Each call names the file that defines the function, so functions of an included file show that
  file. Inline macros have no calls and do not appear. Needs a trace, so add `-vvv` or `-vvvv`:

```text
[74] 0x7FA9385bE102ac3EAc297483Dd6233D62b3e1496::fallback()
  ├─ [59] Main.huff::DOUBLE(0x3)
  │   ├─ [23] SafeMath.huff::ADD(0x3, 0x3)
  │   │   └─ ← 0x6
  │   └─ ← 0x6
  └─ ← [Stop]
```
* `-v`, `-vv`, ...: Adds detail to the `list` report, with the same levels as `forge test`:

| Level    | Prints                                               |
|----------|------------------------------------------------------|
| `-vv`    | console logs of all tests                            |
| `-vvv`   | execution traces of failing tests                    |
| `-vvvv`  | execution traces of all tests                        |
| `-vvvvv` | execution traces of all tests, with storage changes  |

Without `-v`, the report shows whether each test passed, its gas, and why a failing test failed.

#### EVM options
`hnc test` takes the EVM options of `forge test`, and reads the `foundry.toml` of the project like
`forge` does. See `hnc test --help` for the full list. The most common ones:

| Option                                   | Effect                                                                        |
|------------------------------------------|-------------------------------------------------------------------------------|
| `-r`, `--rpc-url <URL>`                  | Runs the tests on a fork of this chain                                        |
| `--fork-block-number <BLOCK>`            | Forks at this block                                                           |
| `--sender <ADDRESS>`                     | Sends the tests from this address                                             |
| `--initial-balance <WEI>`                | The balance the sender starts with                                            |
| `--hardfork <NAME>`                      | Runs the tests with the rules of this hardfork; `-e` overrides it             |
| `--gas-limit <GAS>`                      | The gas limit of the block and of each test transaction                       |
| `--chain`, `--block-number`, `--block-timestamp`, `--gas-price`, `--base-fee`, ... | Set the environment of every test   |
| `--ffi`                                  | Allows the `vm.ffi` cheatcode                                                 |
| `--no-isolate`                           | Turns off call isolation. It is on by default, as in `forge test`, and meters each call a test makes as its own transaction |
| `--live-logs`                            | Prints console logs while the tests run                                       |

Decorators such as `caller(..)` or `block_number(..)` override these values for a single test.

Example:
```shell
hnc ./src/Swap.huff test -r $MAINNET_RPC --fork-block-number 21758848 --target-address 0x738e... -vvvv
```
