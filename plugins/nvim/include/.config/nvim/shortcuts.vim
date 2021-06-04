" Shortcuts

let mapleader = "\<Space>"

" ; is :
nnoremap ; :

" fzf
nmap <Leader><Space> :Buffers<CR>
nmap <C-P> :Files<CR>

" C-Space reload completion list
inoremap <silent><expr> <c-space> coc#refresh()

" Select last yanked text
nnoremap <Leader>v `[v`]

" Leader + o creates a blank line above
nmap <Leader>o o<Esc>
nmap <Leader>O O<Esc>

" NERDTree
nmap <silent> ,, :NERDTreeToggle<CR>
nmap <silent> ,. :NERDTreeFind<CR>zz
" Disable C-J, C-K mappings
let g:NERDTreeMapJumpPrevSibling=""
let g:NERDTreeMapJumpNextSibling=""

" Closes current buffer with <Leader>w
nnoremap <silent> <Leader>w :call Close()<CR>

" Toggles zoom between the current buffer
nnoremap <silent> <Leader>tt :call ToggleFullScreen()<CR>

" Goyo
nmap <silent> <Leader>tg :Goyo<CR>

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
nmap <silent> <leader>gs :Git<CR>
nmap <silent> <leader>gp :Git push<CR>
nmap <silent> <leader>gP :Git push<CR>
nmap <silent> <leader>gb :Git blame<CR>

" ALE
" nnoremap <silent> gd :ALEGoToDefinition<CR>
nmap <silent> [l <Plug>(ale_previous_wrap)
nmap <silent> ]l <Plug>(ale_next_wrap)
nmap <Leader>ff <Plug>(ale_fix)

" CoC
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>fc <Plug>(coc-fix-current)
" nmap <leader>ac <Plug>(coc-codeaction)

" Copy filepath + line to clipboard
nmap <silent> <leader>yl :call YankFilenameLine()<CR>
nmap <silent> <leader>yf :call YankFilename()<CR>

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

" Make n always search forward and N backward
nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

" delete without yanking
nnoremap <leader>d "_d
vnoremap <leader>d "_d

" replace currently selected text with default register without yanking it
vnoremap <leader>p "_dP
