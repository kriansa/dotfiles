# Base system helpers plugin
#
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# 2. Load additional files
# Stash your environment variables in ~/.localrc. This means they'll stay out
# of your main dotfiles repository (which may be public, like this one), but
# you'll have access to them in your scripts.
if [[ -a ~/.localrc ]]; then
  source ~/.localrc
fi
