# Local plugins
plugins=(
  # Shell & utilities
  zsh
  quick-cd
  unix
  tilix

  # Terminal fortunes
  fortunes

  # Development utilities
  git
  gnupg

  # Development platforms
  iot
  terraform

  # IDEs
  nvim
  tmux
  emacs

  # Languages
  ruby
  python
  javascript
  lein
  java
)

for plugin in $plugins; do
  antigen bundle $DOTFILES_PATH/plugins $plugin --no-local-clone
done

# Load local (private) packages
test -f "$DOTFILES_PATH/packages.local.zsh" && \
  source "$DOTFILES_PATH/packages.local.zsh"

# autosugestions is like an enhanced autocomplete with suggestions
antigen bundle zsh-users/zsh-autosuggestions

# Load theme
antigen theme $DOTFILES_PATH/plugins/zsh/themes agnoster --no-local-clone

# syntax-highlighting must be loaded after executing compinit command and
# sourcing other plugins
antigen bundle zsh-users/zsh-syntax-highlighting
