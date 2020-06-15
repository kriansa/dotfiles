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

  " Preserve marks that are used to remember start and
  " end position of the last changed or yanked text (`:h '[`).
  let first_char_pos = getpos("'[")
  let last_char_pos = getpos("']")

  silent! w

  call setpos("'[", first_char_pos)
  call setpos("']", last_char_pos)
endfunction

" Callback function to automatically save the current buffer if needed
function! AutoSave()
  if s:GetVar('auto_save', 0) == 0
    return
  end

  WriteBuffer()
endfunction

" Toggles the auto-save feature
function! AutoSaveToggle()
  if g:auto_save >= 1
    let g:auto_save = 0
    echo "(AutoSave) OFF"
  else
    let g:auto_save = 1
    echo "(AutoSave) ON"
  endif
endfunction

" Autosave files when changing files or losing focus
autocmd BufLeave,FocusLost * silent! call AutoSave()
command AutoSaveToggle :call AutoSaveToggle()
