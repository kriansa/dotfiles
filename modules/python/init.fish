# Add path where user pip bins are installed by default
if test (uname) = "Darwin"
  fish_add_path --path --global $HOME/Library/Python/3.9/bin
else
  fish_add_path --path --global $HOME/.local/bin
end
