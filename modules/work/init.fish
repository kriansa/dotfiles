# Import things that can't be shared
source $DOTFILES_PATH/modules/work/private.fish

# Homebrew re-execs itself through `env -i`, keeping only an allowlist of variables
# (HOMEBREW_*, SSH_AUTH_SOCK, …), so the core.sshCommand pin in ~/.config/git/identity never
# reaches its git and it can't fetch the private salsa tap. HOMEBREW_SSH_CONFIG_PATH does
# survive the filter: brew turns it into `ssh -F<file>`, and that file pins the work key.
set --global --export HOMEBREW_SSH_CONFIG_PATH "$HOME/.ssh/config-brew"
