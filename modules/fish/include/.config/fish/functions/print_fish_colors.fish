# based on: https://github.com/fish-shell/fish-shell/issues/3443#issuecomment-253838672

function print_fish_colors --description 'Shows the various fish colors being used'
  set -l clr_list (set -n | grep fish | grep color | grep -v __)
  if test -n "$clr_list"
    set -l bclr (set_color normal)
    set -l bold (set_color --bold)
    echo "┌──────────────────────────────────────┬──────────────────────────────────────┐"
    echo "│ Variable                             │ Definition                           │"
    echo "├──────────────────────────────────────┼──────────────────────────────────────┤"
    for var in $clr_list
      set -l def $$var
      set -l clr (set_color $def)
      or begin
        printf "│ %-36s │ %s%-36s$bclr │\n" "$var" (set_color --bold white --background=red) "$def"
        continue
      end
      printf "│ $clr%-36s$bclr │ $bold%-36s$bclr │\n" "$var" "$def"
    end
    echo "└──────────────────────────────────────┴──────────────────────────────────────┘"
  end
end
