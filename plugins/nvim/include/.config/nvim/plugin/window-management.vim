" Checks whether NERDTree is open
function! IsNerdTreeEnabled()
  return exists('t:NERDTreeBufName') && bufwinnr(t:NERDTreeBufName) != -1
endfunction

" Count how many valid (non-empty) buffers are open
function! CountOpenBuffers()
  let open_buffers = 0

  for buf in getbufinfo({ 'buflisted': 1 })
    if buf.name != ''
      let open_buffers = open_buffers + 1
    endif
  endfor

  return open_buffers
endfunction

" Allows us closing buffers without affecting the windows, unless we are on
" diff mode, quickfix or NERDtree window, which is desirable that we remove
" the windows.
command! Close call Close()
function! Close()
  if exists("b:NERDTree")
    if CountOpenBuffers() > 0
      execute ":NERDTreeClose"
    endif
  elseif &buftype == "quickfix"
    cclose
  elseif &diff || bufname("%") == ".git/index" || &buftype == "help" || &buftype == 'nofile' || (bufname("%") == ".git/COMMIT_EDITMSG" && winnr("$") > 1)
    execute ":bd"
  else
    execute ":BD"

    " Don't let empty windows when there are buffers to be loaded
    if bufname("%") == "" && CountOpenBuffers() > 0
      execute ":bprevious"
    endif
  endif

  if CountOpenBuffers() == 0 && confirm("This is the last buffer. Quit?", "&No (default)\n&Yes", 1) == 2
    qall
  endif
endfunction

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
