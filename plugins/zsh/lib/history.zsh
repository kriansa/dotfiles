# History settings
# ================

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Show history with timestamps
alias history='fc -il 1'

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
# ignore duplication command history list
setopt hist_ignore_dups
# ignore the history when command is prepended with space
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
# share command history data between sessions
setopt share_history

# Load the history search widget
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
