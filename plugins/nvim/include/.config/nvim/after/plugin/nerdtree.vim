" Toggle opens a NERDTree window in the current window.
"
" This is different than the pure :NERDTreeFind because it will close the
" NERDTree if it's open, and it will open a :NERDTree in the cwd if the buffer
" is empty, instead of throwing an error.
command! NERDTreeToggleInCurDir call NERDTreeToggleInCurDir()
function! NERDTreeToggleInCurDir()
  " If NERDTree is open
  if (exists("t:NERDTreeBufName") && bufwinnr(t:NERDTreeBufName) != -1)
    execute ":NERDTreeClose"
  elseif bufname("%") == ""
    execute ":NERDTree"
  else
    execute ":NERDTreeFind"
  endif
endfunction
