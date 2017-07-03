" Shortcuts
"

let mapleader = "\<Space>"

" Move between buffers
nmap <Leader><Space> :CtrlPBuffer<CR>

" Select last yanked text
nnoremap <leader>v `[v`]

" Make CTRL-C in Insert mode trigger `InsertLeave`
inoremap <C-c> <Esc>

" NerdTREE toggle
nmap <silent> <Leader>. :NERDTreeToggle<CR>
nmap <silent> <Leader>; :NERDTreeToggle<CR>
nmap <silent> <Leader>/ :call NERDTreeToggleInCurDir()<CR>
nmap <silent> <Leader>\ :call NERDTreeToggleInCurDir()<CR>
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

" Deoplete tab-complete
inoremap <expr><Tab> pumvisible() ? "\<c-n>" : "\<Tab>"
inoremap <expr><S-Tab> pumvisible() ? "\<c-p>" : "\<S-Tab>"

" Allows us closing buffers without affecting the windows, unless we are on
" diff mode, quickfix or NerdTREE window, which is desirable that we remove
" the windows.
function! CloseBuffer()
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

" Closes current buffer with <Leader>w
nnoremap <silent> <Leader>w :call CloseBuffer()<CR>

" Closes current selected window with <Leader>q
nnoremap <silent> <Leader>q :wincmd q<CR>

" Easier navigation between splits
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>
nnoremap <silent> <C-h> :wincmd h<CR>

" Easy window management
nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>
nnoremap <silent> <Leader>> :exe "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <silent> <Leader>< :exe "vertical resize " . (winwidth(0) * 2/3)<CR>
nnoremap <silent> <Leader>= :wincmd =<CR>

" Disable Ex mode
nnoremap Q <nop>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>

" Toggle paste mode with F2
set pastetoggle=<F2>

" D deletes from the cursor to the end of the line; C changes from the cursor
" to the end of the line. For some reason, however, Y yanks the entire line,
" both before and after the cursor.
nnoremap Y y$
