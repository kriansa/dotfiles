function! IsWritableBuffer()
  let fugitive_status = bufname("%") =~ ".git/index$"
  let fugitive_diff = bufname("%") =~ "^fugitive://"
  let blank_buffer = bufname("%") == ""

  return !(&diff || &modifiable == 0 || &modified == 0 || &buftype == 'nofile' 
        \|| fugitive_status || fugitive_diff || blank_buffer)
endfunction

" Write to the buffer unless we should not
function! WriteBuffer()
  if !IsWritableBuffer()
    return
  endif

  execute ":w"
endfunction
