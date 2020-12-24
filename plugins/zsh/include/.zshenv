# Entry point for ZSH for non-interactive sessions
#
# Here, it wouldn't make sense to load Antigen and all its feature set which is only useful for
# interactive sessions. However, it's important that we load the same plugins that I load normally
# and are not just for the sake of UI, but also functionality, such as building the right $PATH and
# few other environment variables.

# Set the path to this dotfiles
export DOTFILES_PATH=$HOME/.dotfiles

# Load the list of packages
source $DOTFILES_PATH/plugins/zsh/packages.zsh
# Load local (private) list of packages
test -f $DOTFILES_PATH/packages.local.zsh && source $DOTFILES_PATH/packages.local.zsh

# Lastly, before loading any of our local packages, let's completely nulify the effect of compdef,
# as this is not important here
compdef() {}

# Now we load every package described as a local plugin. And differently than .zshrc, we won't load
# any remote plugin, because their nature is all UI-changing, and we're not interested on that when
# in non-interactive sessions.
for plugin in $_local_plugins; do
  source "$DOTFILES_PATH/plugins/$plugin/$plugin.plugin.zsh"
done
