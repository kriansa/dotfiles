" Yank the filename and line to the system clipboard
function! YankFilename()
  let @+ = expand('%')
  echo "Filename copied to clipboard"
endfunction

" Yank the filename and line to the system clipboard
function! YankFilenameLine()
  let @+ = join([expand('%'),  line(".")], ':')
  echo "Filename and line number copied to clipboard"
endfunction
