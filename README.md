# Kriansa's dotfiles

My dotfiles are using [ZSH](http://zsh.sourceforge.net/) for shell and
[Antigen](https://github.com/zsh-users/antigen) for plugin management.

## Installation

1. Clone this project to `~/.dotfiles`

```shell
$ git clone git@github.com:kriansa/dotfiles.git ~/.dotfiles
```

2. Run setup from the [computer-setup](https://github.com/kriansa/computer-setup)

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
