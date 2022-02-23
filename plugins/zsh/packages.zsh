# Local plugins
_local_plugins=(
  # Shell & utilities
  zsh
  projects
  unix
  lastpass

  # Dotfiles maintainance
  meta

  # Terminal fortunes
  fortunes

  # Development utilities
  git
  gnupg
  devops

  # IDEs
  nvim
  emacs

  # Languages
  ruby
  javascript
  python
  java
)

# Remote plugins
_remote_plugins=(
  # autosugestions is like an enhanced autocomplete with suggestions
  zsh-users/zsh-autosuggestions

  # Add extra completions
  zsh-users/zsh-completions

  # Use Pure theme
  mafredri/zsh-async
  sindresorhus/pure

  # syntax-highlighting must be loaded after executing compinit command and
  # sourcing other plugins
  zsh-users/zsh-syntax-highlighting
)
