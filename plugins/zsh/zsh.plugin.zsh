# Base system helpers plugin
#
PLUGIN_PATH=$0:A:h

# Used by Pure theme
PURE_CMD_MAX_EXEC_TIME=3

# Load additional files
#
# Stash your environment variables in ~/.localrc. This means they'll stay out
# of your main dotfiles repository (which may be public, like this one), but
# you'll have access to them in your scripts.
if [[ -a ~/.localrc ]]; then
  source ~/.localrc
fi

# Setup aliases
alias sudo="sudo " # Pass aliases to root account
alias rg="rg --hidden --ignore-file=$DOTFILES_PATH/.rgignore"
alias cdo="cd ~/Downloads"

# Load ZSH native functions
autoload -U edit-command-line
zle -N edit-command-line

# Load custom functions
source $PLUGIN_PATH/lib/functions/clipboard.zsh
source $PLUGIN_PATH/lib/functions/cut-buffer.zsh
source $PLUGIN_PATH/lib/functions/prepend-sudo.zsh
source $PLUGIN_PATH/lib/functions/edit-this-folder.zsh
source $PLUGIN_PATH/lib/functions/title.zsh

# Load settings for zsh
source $PLUGIN_PATH/lib/dir-navigation.zsh
source $PLUGIN_PATH/lib/history.zsh
source $PLUGIN_PATH/lib/completion.zsh
source $PLUGIN_PATH/lib/keybindings.zsh
