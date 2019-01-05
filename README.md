# Kriansa computer setup

On this repo, I store instructions to setup my machines as well as shared configuration for common
applications - also called _dotfiles_.

My dotfiles are using [ZSH](http://zsh.sourceforge.net/) for shell and
[Antigen](https://github.com/zsh-users/antigen) for plugin management.

I'm doing my machines bootstrapping using [Ansible](https://www.ansible.com/).

## Available machines

* [Desktop](doc/desktop-install.md)

## Installation

1. Clone this project to `~/.dotfiles`

```shell
$ git clone git@github.com:kriansa/dotfiles.git ~/.dotfiles
```

2. Run `bin/setup`

## Enabling a new ZSH plugin

You'll just have to enable it in the `plugins/zsh/packages.zsh`, following
[Antigen](https://github.com/zsh-users/antigen) guidelines.

## Local environment variables

Environment variables like secrets should be kept inside `$HOME/.localrc` and they'll be loaded
automatically.

## Docs

* [Issues](doc/bugs.md)
* [Shortcuts & Commands](doc/shortcuts.md)
* [Updating](doc/updating.md)

## License

BSD 3-Clause.
