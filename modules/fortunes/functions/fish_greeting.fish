function fish_greeting
  cat $HOME/Dropbox/Fortunes/*.txt | sort --random-sort | head -1 | cowsay
end
