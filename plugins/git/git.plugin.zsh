# Root plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# 2. Useful aliases
alias g='git'

# 3. Enable gh autocompletion
eval "$(gh completion --shell zsh)"
