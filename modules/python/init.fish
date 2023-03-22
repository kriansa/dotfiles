function _python_add_path --on-event modify_path
  # Add ~/.local/bin to path - this is where user pip bins are installed by default
  fish_add_path --path --global $HOME/.local/bin
end
