# git-brunch [![Travis](https://travis-ci.org/andys8/git-brunch.svg?branch=master)](https://travis-ci.org/andys8/git-brunch) ![Actions](https://github.com/andys8/git-brunch/workflows/CI/badge.svg)

A git branch checkout command-line tool

![screenshot](https://raw.githubusercontent.com/andys8/git-brunch/master/screenshot.png)

## Features

- Checkout local or remote branch
- Rebase onto a branch
- Search for a branch
- Delete a branch
- Fetch / Update

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

### [Nix](https://nixos.org/nix)

```sh
nix-env -i git-brunch
```

### [Stack](https://haskellstack.org)

```sh
stack install git-brunch # --resolver=lts-16.11
```

### Install from source

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
