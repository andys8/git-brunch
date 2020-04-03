# git-brunch [![Build Status](https://travis-ci.org/andys8/git-brunch.svg?branch=master)](https://travis-ci.org/andys8/git-brunch)

A git checkout and rebase command-line tool

![screenshot](https://raw.githubusercontent.com/andys8/git-brunch/master/screenshot.png)

## Usage

Run `git-brunch` or `git brunch`.

### Git alias (optional)

```sh
git config --global alias.b brunch
```

## Installation

### Download binary

1. Download from [releases](https://github.com/andys8/git-brunch/releases)
1. Rename to `git-brunch`
1. `chmod +x git-brunch`
1. Add to `PATH`

### Arch Linux

`git-brunch` is in the [AUR](https://aur.archlinux.org/packages/git-brunch).
Install it with e.g. `yay -S git-brunch` or `pamac install git-brunch`.

### [Stack](https://haskellstack.org)

#### Install

```sh
stack install git-brunch # --resolver=lts-14.16
```

#### Clone and install from source

```sh
git clone https://github.com/andys8/git-brunch.git
cd git-brunch
stack install
```

### [Nix](https://nixos.org/nix)

#### Install

```sh
nix-env -f "<nixpkgs>" -iA haskellPackages.git-brunch
```

#### Clone and install with `nix`

```sh
git clone https://github.com/andys8/git-brunch.git
cd git-brunch
nix-env -if .
```

## Development

### Run application

```shell
stack build --exec git-brunch
```

### Run tests

```shell
stack test --file-watch
```

### Build statically linked

```shell
stack install --flag git-brunch:static
```

### Generate nix

```sh
cabal2nix --shell . > default.nix
```
