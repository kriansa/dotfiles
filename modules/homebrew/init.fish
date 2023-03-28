function _homebrew_add_path --on-event modify_path
  # Clean homebrew path values previously set by homebrew
  # See: https://github.com/Homebrew/brew/blob/master/Library/Homebrew/cmd/shellenv.sh
  for path_var in PATH MANPATH INFOPATH
    set i 1
    for dir in $$path_var
      if string match --quiet --index "/opt/homebrew*" $dir
        set --erase $path_var\[$i\]
      else
        set i (math $i + 1)
      end
    end
  end

  # Add brew to PATH
  eval (/opt/homebrew/bin/brew shellenv)
end
