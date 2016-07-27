# 1. Exports
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# 2. Load commands
if (( $+commands[pyenv] )); then
  eval "$(pyenv init -)"
fi
