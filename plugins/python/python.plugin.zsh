# 1. Exports
if [[ "$OSTYPE" == darwin* ]]; then
  # Add brew bin path to Python 3 when on Mac
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"

  # Add the user package bin path
  # This path is obtainable through the following command:
  # $ python -m site --user-base
  export PATH="$HOME/Library/Python/3.7/bin:$PATH"
fi
