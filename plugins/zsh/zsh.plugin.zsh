# Base system helpers plugin
#
PLUGIN_PATH=$0:A:h

# Set the default user. Used by Agnoster theme
DEFAULT_USER="dpereira"

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
alias ag="ag --hidden --ignore .git"
function dot () { $EDITOR $DOTFILES_PATH }

# Load custom functions
source $PLUGIN_PATH/lib/functions/clipboard.zsh
source $PLUGIN_PATH/lib/functions/cut-buffer.zsh
source $PLUGIN_PATH/lib/functions/expand-with-dots.zsh
source $PLUGIN_PATH/lib/functions/prepend-sudo.zsh
source $PLUGIN_PATH/lib/functions/edit-this-folder.zsh

# Load settings for zsh
source $PLUGIN_PATH/lib/dir-navigation.zsh
source $PLUGIN_PATH/lib/history.zsh
source $PLUGIN_PATH/lib/completion.zsh
source $PLUGIN_PATH/lib/keybindings.zsh
