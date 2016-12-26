# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Load commands
if (( $+commands[rbenv] )); then
  eval "$(rbenv init -)"
fi

# 2. Exports
export PATH="$PLUGIN_PATH/bin:$HOME/.rbenv/bin:$PATH"
