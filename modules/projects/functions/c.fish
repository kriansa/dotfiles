function c --description="Quickly jumps into a project's directory"
  cd "$PROJECTS/$argv[1]"

  if test $status -eq 0; and test -n "$argv[1]"
    tmux rename-window (string trim --chars=/ $argv[1])
  end
end
