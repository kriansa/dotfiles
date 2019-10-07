# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Load nodenv
if (( $+commands[nodenv] )); then
  eval "$(nodenv init -)"
fi

# 2. Exports
export PATH="$PLUGIN_PATH/bin:$HOME/.config/yarn/global/bin:$PATH"
