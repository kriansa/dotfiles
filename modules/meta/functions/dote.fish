function dote --description="Edit dotfiles"
  set prev_name (tmux display-message -p '#W')
  tmux rename-window dotfiles
  $EDITOR "$DOTFILES_PATH"
  tmux rename-window "$prev_name"
end
