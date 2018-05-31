# This small snippet uses ZLE widgets to cut the entire line (deletes and sends
# it to the clipboard).
#
# More on ZLE Widgets: https://sgeb.io/posts/2014/04/zsh-zle-custom-widgets/

function _cut-whole-line {
  zle kill-whole-line
  echo -n $CUTBUFFER | clipcopy
}

zle -N _cut-whole-line
