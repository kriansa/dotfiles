# Import things that can't be shared
source $DOTFILES_PATH/modules/work/private.fish

# Useful aliases
alias pubkey="cat ~/.ssh/id_ed25519.pub | pbcopy && echo Key copied!"

# brew's update.sh forces GIT_SSH_COMMAND ("${GIT_SSH_COMMAND:-ssh} -oBatchMode=yes"),
# which overrides git's core.sshCommand — so brew ignores the id_ed25519 pin in
# ~/.gitconfig-overrides and can't fetch the private salsa tap. Feed the work key to
# brew only; a global export would clobber core.sshCommand everywhere and break pushes
# to personal repos. brew appends BatchMode itself, so we omit it here.
function brew --wraps=brew
    set --local --export GIT_SSH_COMMAND "ssh -o IdentitiesOnly=yes -i ~/.ssh/id_ed25519"
    command brew $argv
end
