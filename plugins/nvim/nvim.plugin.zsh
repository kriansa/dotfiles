# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# Nvim is our preferred editor
export EDITOR='nvim'

# 3. Aliases
alias v=$EDITOR
