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

### Install with `stack`

```sh
stack install --resolver=lts-14 git-brunch
```

### Clone and install with `nix`

```sh
git clone https://github.com/andys8/git-brunch.git
cd git-brunch
nix-env -if default.nix
```

### Clone and install with `stack` from source

```sh
git clone https://github.com/andys8/git-brunch.git
cd git-brunch
stack install
```
