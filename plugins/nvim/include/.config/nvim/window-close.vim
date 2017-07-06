" Allows us closing buffers without affecting the windows, unless we are on
" diff mode, quickfix or NERDtree window, which is desirable that we remove
" the windows.
command! Close call Close()
function! Close()
  if (exists("b:NERDTree"))
    execute ":NERDTreeClose"
  elseif &buftype == "quickfix"
    cclose
  elseif &diff || bufname("%") == ".git/index" || &buftype == "help" || &buftype == 'nofile' || (bufname("%") == ".git/COMMIT_EDITMSG" && winnr("$") > 1)
    execute ":bd"
  else
    execute ":BD"

    " Don't let empty windows when there are buffers to be loaded
    if bufname("%") == ""
      execute ":bprevious"
    endif
  endif
endfunction
