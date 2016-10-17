# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$HOME/.nodenv/bin:$PATH"

# 2. Load commands
if (( $+commands[nodenv] )); then
  eval "$(nodenv init -)"
fi
