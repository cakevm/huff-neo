# `huff-neo-up`

Update or revert to a specific Huff branch with ease.

_Forked from [foundry](https://github.com/foundry-rs/foundry/tree/master/foundryup)._


## Installing

`curl -L get.huff.sh | bash` (installs the `huff-neo-up` installer)

Run `huff-neo-up` to install the latest version of `huff-neo`.


## Usage

To install the **nightly** version:

```sh
huff-neo-up
```

To install a specific **version** (in this case the `nightly` version):

```sh
huff-neo-up --version nightly
```

To install a specific **branch** (in this case the `release/0.3.1` branch's latest commit):

```sh
huff-neo-up --branch release/0.3.1
```

To install a **fork's main branch** (in this case `abigger87/huff-neo`'s main branch):

```sh
huff-neo-up --repo abigger87/huff-neo
```

To install a **specific branch in a fork** (in this case the `patch-10` branch's latest commit in `abigger87/huff-neo`):

```sh
huff-neo-up --repo abigger87/huff-neo --branch patch-10
```

To install from a **specific Pull Request**:

```sh
huff-neo-up --pr 1071
```

To install from a **specific commit**:
```sh
huff-neo-up -C 94bfdb2
```

To install a local directory or repository (e.g. one located at `~/git/huff`, assuming you're in the home directory)
##### Note: --branch, --repo, and --version flags are ignored during local installations.

```sh
huff-neo-up --path ./git/huff
```

---

**Tip**: All flags have a single character shorthand equivalent! You can use `-v` instead of `--version`, etc.

---