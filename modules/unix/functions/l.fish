function l --wraps=ls --description 'List contents of directory, including hidden files in directory using long format'
  exa -lahg --group-directories-first $argv
end
