# Inserts 'sudo ' at the beginning of the line
# ============================================

function _prepend-sudo {
  if [[ "$BUFFER" != sudo\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  else
    BUFFER=$(cut -c 6- <<< "$BUFFER")
  fi
}

zle -N _prepend-sudo
