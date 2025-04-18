function dotrm --argument-names filepath --description="Remove a file from the dotfiles"
  if test "$filepath" = "-h" || test "$filepath" = "--help" || test "$filepath" = ""
    echo "Usage: dotrm <PATH>"
    echo "Remove a file that's currently in your dotfiles"
    return
  end

  if test -z "$filepath"
    echo "File path is required" >&2
    echo "Usage: dotrm <PATH>"
    return 1
  end

  if test (string sub --length 1 "$filepath") != "/"
    set -f filepath "$(pwd)/$filepath"
  end

  if test (string sub --length (string length $DOTFILES_PATH) $filepath) = $DOTFILES_PATH
    echo "Cannot remove a file from the dotfiles path" >&2
    return 1
  end

  if test (string sub --length (string length $HOME) $filepath) != $HOME
    echo "Cannot remove a file from outside your home directory" >&2
    return 1
  end

  if not test -e "$filepath"
    echo "File '$filepath' does not exist" >&2
    return 1
  end

  if not test -L "$filepath"
    echo "File '$filepath' is not a symlink" >&2
    return 1
  end

  set -f modules_path "$DOTFILES_PATH/modules"
  set -f source_path (readlink -f "$filepath")

  if test (string sub --length (string length $modules_path) $source_path) != $modules_path
    echo "File '$filepath' is not in a dotfiles module" >&2
    return 1
  end

  set -f base_to_dotfiles (string sub --start (math (string length $modules_path)+2) $source_path)
  set -f file_parts (string split -m 2 / $base_to_dotfiles)
  set -f base_to_home $file_parts[3]

  command rm -f $HOME/$base_to_home
  command mv $source_path $HOME/$base_to_home
  echo "Moved '$base_to_home' back to \$HOME"
end
