# Kriansa's dotfiles

My dotfiles are just a enhanced way to work with plugins on `oh-my-zsh`. Fork it and change it as you want.

## Prerequisites

* [Zsh](http://www.zsh.org) should be installed (v4.3.9 or more recent). If not pre-installed (`zsh --version` to confirm), check the following instruction here: [Installing-ZSH](https://github.com/robbyrussell/oh-my-zsh/wiki/Installing-ZSH)
* `curl` should be installed
* `git` should be installed
* `brew` should be installed (on Mac OSX)

## Installation

First you have to install the awesome oh-my-zsh:

```
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```

Then you install this project inside your `~/.dotfiles` path and run `bin/setup`.

```
git clone git@github.com:kriansa/dotfiles.git ~/.dotfiles
~/.dotfiles/bin/setup
```

## Core features

Basically, there are some useful `plugins` that work with the well known OMZ. It just adds some enhancements to it, like installer scripts and a basic plugin architecture.

Refer to [dummy plugin docs](sample-plugin/docs.md) when you want to create one.

### Plugins

Based in the idea of separation of concerns, oh-my-zsh provides us a great framework to build on. When you want to create something about a specific topic, create a plugin for it. It will work just like any module, you can enable/disable whenever you want, and are super useful.

## Adding a new plugin

In runtime, when you enable a new oh-my-zsh plugin, it will load the following files:
* `ZSH_CUSTOM/plugins/<plugin>/<plugin>.zsh`
* `ZSH_CUSTOM/<theme>.zsh-theme`

It will also add this dir into `$fpath` array:
* `ZSH_CUSTOM/plugins/<plugin>`

This way, all the files starting with underscore will be loaded automatically by the ZSH autocomplete, therefore you should keep all your `defcomp` files using this pattern (`ZSH_CUSTOM/<plugin>/_plugin`).

If you need a setup script for your plugin, that will run only once, you can create a `install` file inside the plugin folder and add execute permissions on it, like:
* `ZSH_CUSTOM/plugins/<plugin>/install`

And this file will be executed when you run the `~/.dotfiles/bin/setup`. Make sure your executables:

1. Are protected from a double install case. It should detect when it has already run and don't run again.
2. Has a shebang
3. Sources our beautiful `lib/installer_helper.sh` script

Whenever you create a new install script, you must run `bin/setup` in order to execute it.

### Enabling your plugin

You'll just have to enable it in your `.zshrc`, just like any OMZ plugin.

### Adding shared code to your plugins

By default, OMZ will load these files every time you open your shell:
* ZSH_CUSTOM/lib/*.zsh
* ZSH_CUSTOM/*.zsh

So it's useful to keep all common libraries there.

## Local env variables

Environment variables like secrets should be kept inside `.localrc` and they'll be loaded automatically.

By the way, when using `gnupg` plugin, you should the following line to it:

```
# Sets the default GPG Key
export GPG_MAIN_KEY="<FINGERPRINT_OF_YOUR_KEY>"
```
