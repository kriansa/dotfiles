function rm --description "Safe removal to trash avoiding accidents or mistypings"
  for arg in $argv
    # Get current index
    set -l i (math $i + 1)

    if string match --regex --quiet -- "--" $arg
      set -a files $argv[(math $i + 1)..]
      break
    end

    if string match --regex --quiet -- "^-" $arg
      continue
    end

    set -af files $arg
  end

  trash $files || return 1
  printf "$(set_color --bold)Moved to trash.$(set_color normal) "
  echo "Use $(set_color --italic)`command rm`$(set_color normal) if you want to remove permanently."
end
