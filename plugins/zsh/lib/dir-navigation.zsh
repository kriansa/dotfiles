# Directory navigation
# ====================

# LSCOLORS (BSD) and LS_COLORS (Linux) allow us to customize the ls output
# Generator: https://geoff.greer.fm/lscolors/
export LSCOLORS="Gxfxcxdxbxegedabagacad"
export LS_COLORS="di=1;36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43"

if [[ "$OSTYPE" == darwin* ]]; then
  alias ls="ls -G"
elif [[ "$OSTYPE" == linux* ]]; then
  alias ls="ls --color=auto"
fi

# Changing between directories
setopt auto_pushd
setopt pushd_ignore_dups
setopt auto_cd

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

# List directory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'
