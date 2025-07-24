function fish_greeting
  test -r "$HOME/Documents/Fortunes" || return
  cat $HOME/Documents/Fortunes/*.txt | sort --random-sort | head -1 | cowsay
end
