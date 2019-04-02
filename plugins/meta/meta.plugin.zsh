# Root plugin path
PLUGIN_PATH=$0:A:h

# Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# Helper functions
function dot () { $EDITOR "$DOTFILES_PATH" }
function cdot () { cd "$DOTFILES_PATH" }
