# Import things that can't be shared
source $DOTFILES_PATH/modules/work/private.fish

# Useful aliases
alias pubkey="cat ~/.ssh/id_rsa.pub | pbcopy && echo Key copied!"
