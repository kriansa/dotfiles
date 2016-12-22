" Shortcuts
"

let mapleader = "\<Space>"

" Move between buffers
nmap <silent> <Tab> :silent :bnext<CR>
nmap <silent> <S-Tab> :silent :bprev<CR>
nmap <Leader>b :CtrlPBuffer<CR>

" NerdTREE toggle
nmap <silent> <Leader>/ :call NERDTreeToggleInCurDir()<CR>
nmap <silent> <Leader>\ :call NERDTreeToggleInCurDir()<CR>
function! NERDTreeToggleInCurDir()
  " If NERDTree is open in the current buffer
  if (exists("t:NERDTreeBufName") && bufwinnr(t:NERDTreeBufName) != -1)
    execute ":NERDTreeClose"
  else
    execute ":NERDTreeFind"
  endif
endfunction

" Saves with <Leader>s
noremap <silent> <Leader>s :update<CR>

" Closes current buffer with <Leader>-w
noremap <silent> <Leader>w :bd<CR>

" Disable arrow keys (Vim learner)
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>

" D deletes from the cursor to the end of the line; C changes from the cursor
" to the end of the line. For some reason, however, Y yanks the entire line,
" both before and after the cursor.
nnoremap Y y$
