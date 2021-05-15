# git-brunch [![Travis](https://travis-ci.org/andys8/git-brunch.svg?branch=master)](https://travis-ci.org/andys8/git-brunch) ![Actions](https://github.com/andys8/git-brunch/workflows/CI/badge.svg)

A git command-line tool to work with branches

![screenshot](https://raw.githubusercontent.com/andys8/git-brunch/master/screenshot.png)

## Features

- Quickly checkout local or remote branch
- Merge or rebase a branch
- Search for a branch
- Delete a branch
- Fetch / Update

## Usage

Run `git-brunch` or `git brunch`.

### Git alias (optional)

An alias like `git b` (or `gb`) is a good idea to quickly access the tool.

```sh
git config --global alias.b brunch
```

## Installation

The installation is possible in multiple ways, and there are binaries available to download.

### Download binary

1. Download from **[releases](https://github.com/andys8/git-brunch/releases)**
1. Rename the file to `git-brunch`
1. Make it executable with `chmod +x git-brunch`
1. Add to your `PATH`

### Arch Linux

`git-brunch` is in the [AUR](https://aur.archlinux.org/packages/git-brunch)

```sh
yay -S git-brunch
pamac install git-brunch
```

### FreeBSD

`git-brunch` can be installed from the official FreeBSD package repository

```sh
pkg install hs-git-brunch
```

### [Nix](https://nixos.org/nix)

`git-brunch` is part of the nix package manager

```sh
nix-env -i git-brunch
```

### [Stack](https://haskellstack.org)

`git-brunch` can installed with the Haskell build tool stack

```sh
stack install git-brunch # --resolver=lts-17.11
```

### Install from source

`git-brunch` can be installed from source. It can be forked and modified, if you like to.

```sh
git clone https://github.com/andys8/git-brunch
cd git-brunch
stack install
# or nix-env -if .
```

## Development

### Run application

```shell
stack run
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

## Release

- Bump version in `package.yaml` and `default.nix`
- `stack build`
- Create a commit `v0.0.0`
- Create a tag `v0.0.0`
- Push commit and push tag
- Release on github will be created by CI
- Update release description
- `stack upload .`
- Update [AUR](https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=git-brunch#n3)

## Related projects

- [`git-gone`](https://github.com/lunaryorn/git-gone): Lists or removes "gone" branches, that is, local branches which used to have an upstream branch on a remote which is now removed.
- [`lazygit`](https://github.com/jesseduffield/lazygit): Terminal UI for git commands
- [`gitui`](https://github.com/extrawurst/gitui): Terminal UI focused on speed in giant repositories
