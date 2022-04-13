function ls --wraps=exa --description 'List contents of directory'
  set -x LS_COLORS (vivid generate ~/.config/vivid/theme.yml)
  exa $argv
end
