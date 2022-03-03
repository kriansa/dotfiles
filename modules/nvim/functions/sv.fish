function sv --wraps=$EDITOR --description "Opens $EDITOR with sudo"
  sudo -E $EDITOR $argv
end
