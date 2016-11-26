" Shortcuts
"

" NerdTREE as <Leader> + \
map <Leader>\ :NERDTreeToggle<CR>

" Saves with CTRL+S or CMD+S
noremap <C-s> :update<CR>
noremap <D-s> :update<CR>

" Closes current buffer with <Leader>-w
noremap <Leader>w :bd<CR>

" Disable arrow keys (Vim learner)
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>
