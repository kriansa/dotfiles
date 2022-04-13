function l --wraps=exa --description 'List contents of directory, including hidden files in directory using long format'
  ls -lahg --group-directories-first $argv
end
