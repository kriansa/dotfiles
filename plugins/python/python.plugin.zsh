# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Load pyenv
export PATH="$HOME/.pyenv/bin:$PATH"
if (( $+commands[pyenv] )); then
  eval "$(pyenv init -)"
fi

# 2. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"
