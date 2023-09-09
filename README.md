# Kriansa computer setup

This is where I store instructions to setup my machines as well as shared configuration for common
applications - also called _dotfiles_.

My dotfiles are using [Fish](https://fishshell.com/) and I manage them using `bin/dotup`.

I'm doing my machines bootstrapping using [Ansible](https://www.ansible.com/).

## Available machines

* [Desktop/Laptop](doc/arch-install.md)
* [MacOS](doc/macos-install.md)

## Installation

1. Clone this project to `~/.dotfiles`:
  ```shell
  $ git clone git@github.com:kriansa/dotfiles.git ~/.dotfiles
  ```

2. Run `cd .dotfiles && bin/dotup`

## Docs

* [Issues](doc/bugs.md)
* [Shortcuts & Commands](doc/shortcuts.md)

## License

Apache 2.0
