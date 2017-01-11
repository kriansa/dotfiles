# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Load nodenv
export PATH="$HOME/.nodenv/bin:$PATH"
if (( $+commands[nodenv] )); then
  eval "$(nodenv init -)"
fi

# 2. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"
