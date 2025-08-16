# Huff Neo WASM

The [Huff Neo](https://github.com/cakevm/huff-neo) compiler compiled to WebAssembly. This package brings the full power of the Huff compiler to JavaScript environments - the same production-ready compiler used in the CLI tool, now available as a portable WASM module.

- **Complete Huff compiler** - Full lexing, parsing, and bytecode generation
- **Zero dependencies** - Self-contained WASM module with no external requirements
- **Cross-platform** - Works in browsers, Node.js, and other JavaScript environments
- **TypeScript support** - Fully typed API with proper interface definitions

## Requirements

- **ECMAScript 2015 (ES6) or later** - Required for `Map` support
- **Node.js 6.0.0 or later** for server-side usage
- Modern browsers (excludes IE 11)

### TypeScript Configuration

If using TypeScript, ensure your configuration targets ES2015 or later:

```json
{
  "compilerOptions": {
    "target": "ES2015",
    "lib": ["ES2015", "DOM"]
  }
}
```

## Installation

```bash
npm install huff-neo-js
```

## Usage

The compiled package exposes a typed `compile` function:

```typescript
function compile(input: CompilerInput): CompilerOutput
```

### Input Format

```typescript
interface CompilerInput {
  evm_version?: string;                     // EVM version (e.g., "paris")
  sources: string[];                        // Array of file paths to compile
  files: Map<string, string>;               // Map of filename to source code
  construct_args?: string[];                // Constructor arguments
  alternative_main?: string;                // Alternative main macro name
  alternative_constructor?: string;         // Alternative constructor macro name
}
```

### Output Format

```typescript
interface CompilerOutput {
  errors?: string[];                        // Array of compilation errors
  contracts?: Map<string, CompilerArtifact>; // Map of compiled contracts
}

interface CompilerArtifact {
  bytecode: string;                         // Deployment bytecode
  runtime: string;                          // Runtime bytecode
  abi?: Abi;                               // Generated ABI
  constructor_map?: SourceMapEntry[];       // Constructor source map
  runtime_map?: SourceMapEntry[];           // Runtime source map
}
```

**Note:** The API uses JavaScript `Map` objects for collections, requiring ES2015+ support.

### Example Usage

```typescript
import { compile, CompilerInput } from 'huff-neo-js';

const files = new Map([
  ["add.huff", `
    #define function add(uint256,uint256) nonpayable returns (uint256)

    #define macro MAIN() = {
        // Load our numbers from calldata and add them together.
        0x04 calldataload // [number1]
        0x24 calldataload // [number2]
        add               // [number1+number2]
        // Return our new number.
        0x00 mstore // Store our number in memory.
        0x20 0x00 return // Return it.
    }
  `]
]);

const input: CompilerInput = {
  evm_version: "paris",
  sources: ["add.huff"],
  files,
  construct_args: undefined,
  alternative_main: undefined,
  alternative_constructor: undefined
};

const result = compile(input);

if (result.errors) {
  console.error("Compilation errors:", result.errors);
} else {
  console.log("Compiled contracts:", result.contracts);
}
```

Outputs:

```text
{
  errors: undefined,
  contracts: Map(1) {
    'add.huff' => {
      bytecode: '600f8060093d393df36004356024350160005260206000f3',
      runtime: '6004356024350160005260206000f3',
      abi: [Object]
    }
  }
}
```