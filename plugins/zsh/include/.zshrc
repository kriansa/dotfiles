# Entry point for ZSH
# ===================

# Set the path to this dotfiles
export DOTFILES_PATH=$HOME/.dotfiles

# Load Antigen
source $DOTFILES_PATH/antigen.zsh

# Don't load plugins to PATH or fpath automatically
-antigen-load-env () {}

# Add packages.zsh to Antigen check when doing cache optimization
typeset -a ANTIGEN_CHECK_FILES=($DOTFILES_PATH/packages.zsh ~/.zshrc)

# Load my packages
source $DOTFILES_PATH/packages.zsh

# Activate antigen
antigen apply
