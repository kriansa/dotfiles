function c --description="Quickly jumps into a project's directory"
  tmux rename-window (string trim --chars=/ $argv[1])
  cd "$PROJECTS/$argv[1]"
end
