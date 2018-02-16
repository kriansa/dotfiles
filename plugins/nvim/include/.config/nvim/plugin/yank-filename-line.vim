" Yank the filename and line to the system clipboard
function! YankFilenameLine()
  let file_and_line = join([expand('%'),  line(".")], ':')
  silent execute("!printf " . shellescape(file_and_line) . " | pbcopy")
endfunction
