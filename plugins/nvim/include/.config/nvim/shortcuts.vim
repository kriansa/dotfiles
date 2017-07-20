" Shortcuts
"

let mapleader = "\<Space>"

" Move between buffers
nmap <Leader><Space> :CtrlPBuffer<CR>

" Select last yanked text
nnoremap <leader>v `[v`]

" Make CTRL-C in Insert mode trigger `InsertLeave`
inoremap <C-c> <nop>

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

" Closes current selected window with <Leader>q
nnoremap <silent> <Leader>q :wincmd q<CR>

" Easier navigation between splits
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>
nnoremap <silent> <C-h> :wincmd h<CR>

" Make all splits with equal size
nnoremap <silent> <Leader>= :wincmd =<CR>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>

" Winresizer starts with F3
let g:winresizer_start_key = '<F3>'

" Toggle paste mode with F2
set pastetoggle=<F2>

" D deletes from the cursor to the end of the line; C changes from the cursor
" to the end of the line. For some reason, however, Y yanks the entire line,
" both before and after the cursor.
nnoremap Y y$
