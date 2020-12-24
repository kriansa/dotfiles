# Entry point for ZSH (for interactive sessions)
# ==============================================

# Set the path to this dotfiles
export DOTFILES_PATH=$HOME/.dotfiles

# Load Antigen
source $DOTFILES_PATH/plugins/zsh/antigen.zsh

# Don't load plugins to PATH or fpath automatically
-antigen-load-env () {}

# Add packages.zsh to Antigen check when doing cache optimization
typeset -a ANTIGEN_CHECK_FILES=($DOTFILES_PATH/plugins/zsh/packages.zsh ~/.zshrc)

# Load the list of packages
source $DOTFILES_PATH/plugins/zsh/packages.zsh
# Load local (private) list of packages
test -f $DOTFILES_PATH/packages.local.zsh && source $DOTFILES_PATH/packages.local.zsh

# Then we load our local plugins
for plugin in $_local_plugins; do
  antigen bundle $DOTFILES_PATH/plugins $plugin --no-local-clone
done

# And then we load the remote plugins
for plugin in $_remote_plugins; do
  antigen bundle $plugin
done

# Activate antigen
antigen apply
