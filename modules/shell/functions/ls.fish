function ls --wraps=eza --description 'List contents of directory'
  if not set -q LS_COLORS
    set --global --export LS_COLORS (vivid generate ~/.config/vivid/theme.yml)
  end

  eza $argv
end
