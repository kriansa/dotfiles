function dote --description="Edit dotfiles"
  tmux rename-window dotfiles
  $EDITOR "$DOTFILES_PATH"
end
