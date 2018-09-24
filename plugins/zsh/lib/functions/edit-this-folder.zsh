# Edits the current folder using $EDITOR
# ======================================

function _edit-this-folder {
  $EDITOR .
}

zle -N _edit-this-folder
