# Import things that can't be shared
source $DOTFILES_PATH/modules/work/private.fish

# Binaries from kubectl krew
fish_add_path --path --global $HOME/.krew/bin

# Useful aliases
alias pubkey="cat ~/.ssh/id_rsa.pub | pbcopy && echo Key copied!"
