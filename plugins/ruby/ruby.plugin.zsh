# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$HOME/.rbenv/bin:$PLUGIN_PATH/bin:$PATH"

# 2. Load commands
if (( $+commands[rbenv] )); then
  eval "$(rbenv init -)"
fi
