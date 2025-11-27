# Import things that can't be shared
source $DOTFILES_PATH/modules/work/private.fish

# Useful aliases
alias pubkey="cat ~/.ssh/id_ed25519.pub | pbcopy && echo Key copied!"
