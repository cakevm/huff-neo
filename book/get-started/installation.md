## Installing Huff

The [Huff Neo Compiler](https://github.com/cakevm/huff-neo) is built in Rust to create an extremely performant experience compiling huff.

Installation of the compiler is similar to that of [Foundry](https://github.com/foundry-rs/foundry).

First, install `hnc-up`, a version control manager for the Huff Compiler:

```shell
curl -L https://raw.githubusercontent.com/cakevm/huff-neo/main/hnc-up/install | bash
```

_NOTE: This installs the `hnc-up` binary, but does not guarantee it is added to your path. If you get an error like `hnc-up: command not found`, you will need to source your path by running `source ~/.bashrc` or `source ~/.zshrc`. Alternatively, you can open a new terminal window._

Now, with `hnc-up` installed and in your path, you can simply run `hnc-up` to install the latest stable version of `hnc` (the huff compiler).

### On Windows, build from the source

If you use Windows, you need to build from the source to get huff.

Download and run `rustup-init` from [rustup.rs](https://win.rustup.rs/x86_64). It will start the installation in a console.

If you encounter an error, it is most likely the case that you do not have the VS Code Installer which you can [download here](https://visualstudio.microsoft.com/downloads/) and install.

After this, run the following to build huff from the source:

```shell
cargo install --git https://github.com/cakevm/huff-neo.git hnc --bins --locked
```

To update from the source, run the same command again.

🎉 TADA, Huff is installed! 🎉

To verify for yourself that it's installed, run `hnc --help` to view the help menu.

To get started compiling Huff Contracts, check out [compiling](compiling.md).

To diver deeper into the compiler's cli abilities, check out the [cli docs](../cli.md).
