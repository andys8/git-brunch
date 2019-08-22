# git-brunch [![Build Status](https://travis-ci.org/andys8/git-brunch.svg?branch=master)](https://travis-ci.org/andys8/git-brunch)

A git checkout command-line tool

![screenshot](https://raw.githubusercontent.com/andys8/git-brunch/master/screenshot.png)

## Usage

Run `git-brunch` or `git brunch`.

## Git alias (optional)

```sh
git config --global alias.b brunch
```

## Installation

### Download binary

Download from [releases](https://github.com/andys8/git-brunch/releases), rename to `git-brunch` and add to `PATH`.

### [Stack](https://haskellstack.org)

#### Install

```sh
stack install git-brunch # --resolver=lts-14
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
