function fish_greeting
  cat $HOME/Documents/Fortunes/*.txt | sort --random-sort | head -1 | cowsay
end
