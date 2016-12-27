# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Load rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
if (( $+commands[rbenv] )); then
  eval "$(rbenv init -)"
fi

# 2. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"
