function dotadd --argument-names filepath module --description="Add a file to the dotfiles"
  if test "$filepath" = "-h" || test "$filepath" = "--help" || test "$filepath" = ""
    echo "Usage: dotadd <PATH> <MODULE_NAME>"
    echo "Move the given path to your dotfile's module. It must be located at your \$HOME"
    return
  end

  if test -z "$filepath"
    echo "File path is required" >&2
    echo "Usage: dotadd <PATH> <MODULE_NAME>"
    return 1
  end

  if test -z "$module"
    echo "Module name is required" >&2
    echo "Usage: dotadd <PATH> <MODULE_NAME>"
    return 1
  end

  if test (string sub --length 1 "$filepath") != "/"
    set -f filepath "$(pwd)/$filepath"
  end

  if test (string sub --length (string length $DOTFILES_PATH) $filepath) = $DOTFILES_PATH
    echo "Cannot add a file from the dotfiles path" >&2
    return 1
  end

  if test (string sub --length (string length $HOME) $filepath) != $HOME
    echo "Cannot add a file from outside your home directory" >&2
    return 1
  end

  if not test -e "$filepath"
    echo "File '$filepath' does not exist" >&2
    return 1
  end

  if test -L "$filepath"
    echo "File '$filepath' is a symlink" >&2
    return 1
  end

  if not test -d "$DOTFILES_PATH/modules/$module"
    echo "Module '$module' does not exist" >&2
    return 1
  end

  set -f module_path "$DOTFILES_PATH/modules/$module"
  set -f basepath (string sub --start (math (string length $HOME)+2) $filepath)

  if test -e "$module_path/include/$basepath"
    echo "File $basepath already exists in $module_path/include" >&2
    return 1
  end

  set -f destdir (dirname "$module_path/include/$basepath")
  if not test -d $destdir
    mkdir -p "$destdir"
  end

  command mv "$filepath" "$module_path/include/$basepath"
  command ln -s "$module_path/include/$basepath" "$filepath"
  echo "Linked '$basepath' to module '$module'"
end
