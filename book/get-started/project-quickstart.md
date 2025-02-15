# Huff Project Template

Huff has a [project template](https://github.com/cakevm/huff-neo-project-template) to make it easier to get started writing Huff contracts!


## Using the Template

After navigating to https://github.com/cakevm/huff-neo-project-template, you can click the [Use this template](https://github.com/cakevm/huff-neo-project-template/generate) button on the top right of the repository to create a new repository containing all the template's code.

Once you've cloned and entered into your repository, you need to install the necessary dependencies. In order to do so, simply run:

```bash
forge install
```

Then, you can build and/or run tests with the following commands:

```bash
forge build
forge test
```

Inside the template, there is a contract in the `src/` directory (the default location for huff contracts) called `src/SimpleStore.huff`.  This contract demonstrates a simple contract to set and get values stored in the contract, with the functions being (as defined by the function annotations at the top of the contract):

```javascript
function setValue(uint256);
function getValue() view returns (uint256);
```

Inside the `test/` directory, there are tests for the `src/SimpleStore.huff` contract in `test/SimpleStore.t.sol`. Since [Foundry](https://github.com/foundry-rs/foundry) doesn't natively support compiling huff code, huff projects have to use the [foundry-huff](https://github.com/huff-language/foundry-huff) library to be able to compile huff code using `forge` commands.

_NOTE: In order to compile huff code, foundry-huff behind the scenes need the [Huff Neo Compiler](https://github.com/cakevm/huff-neo) to be installed._

Returning back to our test contract `test/SimpleStore.t.sol`, we can run the following command to run all tests: `forge test`.


## Other Template Features

Once you have created a new repository from the [project template](https://github.com/cakevm/huff-neo-project-template), there are a few things to note before digging in and writing your huff contracts.

The [foundry.toml](https://github.com/cakevm/huff-neo-project-template/blob/main/foundry.toml) file located in the root of the project template, contains the configuration for using the `forge` toolchain.

Inside [./.github/workflows](https://github.com/cakevm/huff-neo-project-template/tree/main/.github/workflows) there is a GitHub action file that will run CI using the [Foundry toolchain](https://github.com/foundry-rs/foundry-toolchain) and the [huff-toolchain](https://github.com/huff-language/huff-toolchain).
