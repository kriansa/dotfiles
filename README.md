# Kriansa environment setup

On this repo, I store instructions to setup my machines as well as shared
configuration for common applications - also called _dotfiles_.

My dotfiles are using [ZSH](http://zsh.sourceforge.net/) for shell and
[Antigen](https://github.com/zsh-users/antigen) for plugin management.

I'm doing my machines bootstrapping using [Ansible](https://www.ansible.com/).

## Installation

1. Clone this project to `~/.dotfiles`

```shell
$ git clone git@github.com:kriansa/dotfiles.git ~/.dotfiles
```

2. Run `bin/setup`

## Enabling a new plugin

You'll just have to enable it in the `packages.zsh`, following
[Antigen](https://github.com/zsh-users/antigen) guidelines.

## Local environment variables

Environment variables like secrets should be kept inside `.localrc` and they'll
be loaded automatically.

By the way, when using `gnupg` plugin, you should the following line to it:

```shell
# Sets the default GPG Key
export GPG_MAIN_KEY="<FINGERPRINT_OF_YOUR_KEY>"
```

## How-to setup

* [Desktop](doc/desktop-install.md)

## License

BSD 3-Clause.
