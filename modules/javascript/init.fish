# Add ~/.config/yarn/global/bin to path - this is where yarn global packages are installed
fish_add_path --path --global $HOME/.config/yarn/global/bin

if type -q nodenv and status is-interactive
  source (nodenv init - | psub)
end
