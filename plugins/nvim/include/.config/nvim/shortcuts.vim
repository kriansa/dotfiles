" Shortcuts
"

let mapleader = "\<Space>"

" Move between buffers
nmap <Leader><Space> :CtrlPBuffer<CR>

" Select last yanked text
nnoremap <leader>v `[v`]

" Leader + o creates a blank line above
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>

" NerdTREE toggle
nmap <silent> <Leader>. :NERDTreeToggle<CR>
nmap <silent> <Leader>; :NERDTreeToggle<CR>
nmap <silent> <Leader>/ :call NERDTreeToggleInCurDir()<CR>
nmap <silent> <Leader>\ :call NERDTreeToggleInCurDir()<CR>

" Deoplete tab-complete
inoremap <expr><Tab> pumvisible() ? "\<c-n>" : "\<Tab>"
inoremap <expr><S-Tab> pumvisible() ? "\<c-p>" : "\<S-Tab>"

" Closes current buffer with <Leader>w
nnoremap <silent> <Leader>w :call Close()<CR>

" Toggles zoom between the current buffer
nnoremap <silent> <Leader>f :ZoomWinTabToggle<CR>

" Closes current selected window with <Leader>q
nnoremap <silent> <Leader>q :wincmd q<CR>

" Quit using Q
map Q :qa<CR>

" On iTerm2, I've remmaped Cmd + Shift + h,j,k,l to send a Hex code
" corresponding to Ctrl + \ + h,j,k,l so I can use Cmd + Shift bindings
nnoremap <silent> <C-\>h :wincmd h<CR>
nnoremap <silent> <C-\>j :wincmd j<CR>
nnoremap <silent> <C-\>k :wincmd k<CR>
nnoremap <silent> <C-\>l :wincmd l<CR>

if has('nvim')
  " Maps Cmd + Shift + h,j,k,l
  tnoremap <silent> <C-\>h <c-\><c-n>:wincmd h<CR>
  tnoremap <silent> <C-\>j <c-\><c-n>:wincmd j<CR>
  tnoremap <silent> <C-\>k <c-\><c-n>:wincmd k<CR>
  tnoremap <silent> <C-\>l <c-\><c-n>:wincmd l<CR>

  " Alt-arrows should move through words and Command-Backspace deletes the
  " whole line
  tnoremap <A-Left> <Esc>b
  tnoremap <A-Right> <Esc>f
  tnoremap <D-Bs> <Esc>q
endif

" Shift + J join lines. Shift + K should split lines
nnoremap <silent> <S-K> i<CR><ESC>

" Fugitive
nmap <silent> <leader>gs :Gstatus<CR>

" Make all splits with equal size
nnoremap <silent> <Leader>= :wincmd =<CR>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>

" Winresizer starts with <Leader>+e
let g:winresizer_start_key = '<Leader>e'

" Neoterm
nnoremap <silent> <Leader>t :Ttoggle<CR>

" Toggle paste mode with F2
set pastetoggle=<F2>

" D deletes from the cursor to the end of the line; C changes from the cursor
" to the end of the line. For some reason, however, Y yanks the entire line,
" both before and after the cursor.
nnoremap Y y$
