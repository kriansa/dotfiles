# Allow expansion with dots
# =========================

function expand-or-complete-with-dots() {
  # toggle line-wrapping off and back on again
  [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
  print -Pn "%{%F{red}...%f%}"
  [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam

  zle expand-or-complete
  zle redisplay
}

zle -N expand-or-complete-with-dots
