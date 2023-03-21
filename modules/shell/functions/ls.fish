function ls --wraps=exa --description 'List contents of directory'
  if set -q LS_COLORS
    set --global --export LS_COLORS (vivid generate ~/.config/vivid/theme.yml)
  end

  exa $argv
end
