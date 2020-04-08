# 1. Exports
export ALTERNATE_EDITOR=$EDITOR

# 2. Aliases
function e() {
  if ! pgrep emacs > /dev/null 2>&1; then
    emacs --daemon
  fi

  emacsclient -cn "$@"
}
