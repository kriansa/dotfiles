# 1. Exports
# Add brew bin path to Python 3 when on Mac
if [[ "$OSTYPE" == darwin* ]]; then
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
fi
