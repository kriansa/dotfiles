" Shortcuts

let mapleader = "\<Space>"

" ; is :
nnoremap ; :

" Don't yank pasted text on visual mode
vnoremap <silent> p :<C-u>call SmartPaste()<CR>
vnoremap <silent> P :<C-u>call SmartPaste()<CR>

" Move between buffers
nmap <Leader><Space> :CommandTBuffer<CR>
nmap <C-P> :CommandT<CR>

" Autofix with ALE
nmap <Leader>f :ALEFix<CR>

" Select last yanked text
nnoremap <Leader>v `[v`]

" Leader + o creates a blank line above
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>

" NERDTree
nmap <silent> ,, :NERDTreeToggle<CR>
nmap <silent> ,. :call NERDTreeToggleInCurDir()<CR>

" Closes current buffer with <Leader>w
nnoremap <silent> <Leader>w :call Close()<CR>

" Toggles zoom between the current buffer
nnoremap <silent> <Leader>t :call ToggleFullScreen()<CR>

" Closes current selected window with <Leader>q
nnoremap <silent> <Leader>q :wincmd q<CR>

" Quit using Q
map Q :wqa<CR>

" Use CTRL-h,j,k,l to move between splits
nnoremap <silent> <C-h> :wincmd h<CR>
nnoremap <silent> <C-j> :wincmd j<CR>
nnoremap <silent> <C-k> :wincmd k<CR>
nnoremap <silent> <C-l> :wincmd l<CR>

" Alt-arrows should move through words and Command-Backspace deletes the
" whole line
tnoremap <A-Left> <Esc>b
tnoremap <A-Right> <Esc>f
tnoremap <D-Bs> <Esc>q

" Shift + J join lines. Shift + K should split lines
nnoremap <silent> <S-K> i<CR><ESC>

" Fugitive
nmap <silent> <leader>gs :Gstatus<CR>
nmap <silent> <leader>gp :Gpush<CR>
nmap <silent> <leader>gP :Gpush<CR>
nmap <silent> <leader>gb :Gblame<CR>

" ALE
nnoremap <silent> gd :ALEGoToDefinition<CR>
nmap <silent> [l <Plug>(ale_previous_wrap)
nmap <silent> ]l <Plug>(ale_next_wrap)

" Copy filepath + line to clipboard
nmap <silent> <leader>fl :call YankFilenameLine()<CR>

" Make all splits with equal size
nnoremap <silent> <Leader>= :wincmd =<CR>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>

" Winresizer starts with <Leader>+e
let g:winresizer_start_key = '<Leader>e'

" Align GitHub-flavored Markdown tables
au FileType markdown vmap <Leader><Bslash> :EasyAlign*<Bar><Enter>

" D deletes from the cursor to the end of the line; C changes from the cursor to the end of the
" line. For some reason, however, Y yanks the entire line, both before and after the cursor. This
" map makes Y yank from the cursor to EOL.
nnoremap Y y$
