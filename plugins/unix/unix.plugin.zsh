# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$HOME/.local/bin:$PATH"

# 2. Function to help SSH'ing using tun0
function tunssh {
  iftun tun0 SSH_AUTH_SOCK="$SSH_AUTH_SOCK" ssh "$@"
}

function tunscp {
  iftun tun0 SSH_AUTH_SOCK="$SSH_AUTH_SOCK" scp "$@"
}

# Add this plugin path to the fpath completion list
fpath+=($PLUGIN_PATH/completions)
