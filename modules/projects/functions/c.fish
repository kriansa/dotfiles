function c --description="Quickly jumps into a project's directory"
  test -n "$argv[1]"; and tmux rename-window (string trim --chars=/ $argv[1])
  cd "$PROJECTS/$argv[1]"
end
