# Plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"
export LPASS_PINENTRY=$PLUGIN_PATH/bin/pinentry-gui
