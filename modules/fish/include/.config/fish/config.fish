# Add ~/.bin to path
fish_add_path --path --move --global $HOME/.bin

if status is-interactive
  # Load theme
  source (/usr/bin/starship init fish --print-full-init | psub)
end
