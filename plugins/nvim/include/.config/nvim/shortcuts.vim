" Shortcuts
"

let mapleader = "\<Space>"

" Move between buffers
nmap <silent> <Tab> :silent :bnext<CR>
nmap <silent> <S-Tab> :silent :bprev<CR>
nmap <Leader><Space> :CtrlPBuffer<CR>

" Select last yanked text
nnoremap <leader>v `[v`]

" NerdTREE toggle
nmap <silent> <Leader>. :NERDTreeToggle<CR>
nmap <silent> <Leader>; :NERDTreeToggle<CR>
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

" Deoplete tab-complete
inoremap <expr><Tab> pumvisible() ? "\<c-n>" : "\<Tab>"
inoremap <expr><S-Tab> pumvisible() ? "\<c-p>" : "\<S-Tab>"

" Closes current buffer with <Leader>w
nnoremap <silent> <Leader>w :bd<CR>

" Closes current selected window with <Leader>q
nnoremap <silent> <Leader>q <C-w>q

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

" Disable arrow keys (Vim learner)
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Return to normal mode while in insert mode with jk
inoremap jk <C-c>

" Disable esc to exit visual mode
inoremap <ESC> <NOP>

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
