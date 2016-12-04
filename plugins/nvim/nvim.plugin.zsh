# Nvim plugin
#

# Make nvim our preferred editor for local and vim for remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi
