# Add path where user pip bins are installed by default
if test (uname) = "Darwin"
  # Right now, it's better to be accurate about the path than having a faster initialization
  fish_add_path --path --global (python3 -m site --user-base)/bin
end

fish_add_path --path --global $HOME/.local/bin
