function _javascript_add_path --on-event modify_path
  # Add ~/.config/yarn/global/bin to path - this is where yarn global packages are installed
  fish_add_path --path --global $HOME/.config/yarn/global/bin
end
