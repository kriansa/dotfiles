# Plugin development guide

This is a brief guide to describe the architecture behind a plugin.

## Folders

### Root

Here you'll put your installer, docs, compdefs, gitignores and the entry point
of your plugin - and perhaps everything else that doesn't fit elsewhere.

### Bin

Everything inside this folder should have execute permissions. And thus, all
files listed here are meant to be binaries.

### Data

Usually a plugin may have data to be imported, such as seeds or templates. This
is where it should be stored.

> e.g: gpg-public-keys.asc

### Include

Documents inside that folder should match docs inside your $HOME folder.

> e.g: include/.gnupg/gpg.conf should point to $HOME/.gnupg/gpg.conf and so on.

## Entry point (<plugin>.plugin.zsh)

This is the main entry file when your plugin gets loaded. It's called every time
a new console session is invoked, so this should be nothing heavy or it'll will
impact the load time of your shell.

You'll manually add the bin to your $PATH variable, create functions, set
aliases and such.

## Installer (install)

This is where you'll describe what is needed to install your plugin. Usually is
packages installations, set configurations, etc.
