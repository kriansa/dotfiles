# Keybindings & Commands

Below there are the commands and keybindings so I can remember them.

## ZSH

### Shortcuts

* __CTRL-X + CTRL-E__: Edits the current command using `$EDITOR`
* __CTRL-E__: Edits the current folder using `$EDITOR` (saves you from `v .`)
* __CTRL-S__: Prepends (or removes) the current command with `sudo`
* __CTRL-U__: Cuts the entire command into the clipboard

### Commands

* __dot__: Opens the dotfiles using `$EDITOR`
* __cdot__: CD into the dotfiles path
* __mcd__ `<dir>`: Creates and `cd` into the created directory
* __title__ `<name>`: Sets the title of the current window
* __c__ `<project>`: Jumps into a subfolder of `$PROJECTS`
* __clipcopy__ & __clippaste__: Copy from stdin to clipboard / paste into stdout
* __pubkey__ `[<output>]`: Copy my SSH public key into the clipboard. If you pass any parameter,
  sends it to stdout instead
* __docker-clear__: Clear the system from dangling images & stopped containers
* __extract__ `<file>`: Extracts any file
* __compress__ `<path>`: Quickly compress a directory to .tar.gz or a file to .zip
* __getchmod__ `<path>`: Gets the octal chmod number of the given path
* __headers__ `<URL>`: Gets the headers from the HTTP URL.
* __imgcat__ `<file>`: Prints a image to the console
* __dotenv__ : Exports the .env variables onto the current shell
* __gpg-backup__ `<import|export> [<path>]`: Saves/load the current gpg keyring into the backup dir
* __dump-dotenv-prefs__ : Save settings from various applications to my dotenv

## Git

* __g st__: Alias for `status`
* __g co__: Alias for `checkout`
* __g cm__: Alias for `commit`
* __g uncommit__: Alias for `reset --soft HEAD^`
* __g wip__: Create a commit with all staged & unstaged stuff so you can move between branches.
* __g unwip__: Restore the branch to a stage when you did a `g wip`
* __g graph__: Show a graph of commits in the console

## NVIM

#### [Source](plugins/nvim/include/.config/nvim/shortcuts.vim)

* __\<Leader\>-.__ or __\<Leader\>-;__: Toggle NERDTree
* __\<Leader\>-/__ or __\<Leader\>-\\__: Toggle NERDTree in current directory
* __\<Leader\>-fl__: Copies the current file:line into the clipboard
* __\<Leader\>-\\__: _on markdown, visual mode_ Align github-flavored markdown tables
* __\<Leader\>-t__: Toggle full screen mode

## Emacs

#### [Source](plugins/emacs/include/.config/emacs/modules/mine/shortcuts.el)

* __C-SPC__: Trigger autocomplete, then accept the suggestion.
