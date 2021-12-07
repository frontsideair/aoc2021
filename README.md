# [Advent of Code 2021](https://adventofcode.com/2021) in [Haskell](https://www.haskell.org)

## How to run?

You should have [nix](https://nixos.org/download.html) installed with [flakes](https://nixos.wiki/wiki/Flakes) enabled.

```sh
cd day-01
nix run
```

## How to hack?

You should have:

- [nix](https://nixos.org/download.html) with [flakes](https://nixos.wiki/wiki/Flakes)
- [direnv](https://direnv.net) (optional)

```sh
cd day-01
direnv allow
cabal run
ghcid
```

If you don't have direnv, open a shell with development tools by running `nix develop`.

### Editor support

For Visual Studio Code, you need these extensions:

- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
- [direnv](https://marketplace.visualstudio.com/items?itemName=Rubymaniac.vscode-direnv) (optional)

If you have `direnv`, you need to allow the `.envrc` from the command palette. If not, start Code from a development shell with `nix develop`.
