" Custom formatter to add a space around the filename
"
function! airline#extensions#tabline#formatters#custom#format(bufnr, buffers)
  " Call original formatter.
  let originalFormatter = airline#extensions#tabline#formatters#default#format(a:bufnr, a:buffers)
  return ' ' . originalFormatter . ' '
endfunction
