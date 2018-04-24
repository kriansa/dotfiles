# Inserts 'sudo ' at the beginning of the line
# ============================================

function _prepend-sudo {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}

zle -N _prepend-sudo
