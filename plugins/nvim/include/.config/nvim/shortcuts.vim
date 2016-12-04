" Shortcuts
"

let mapleader = "\<Space>"

" Move between buffers
map <silent> <Leader>n :bnext<CR>
map <silent> <Leader>p :bprev<CR>
map <Leader>b :b<Space>

" NerdTREE as <Leader> + /
map <Leader>/ :NERDTreeToggle<CR>

" NerdTREE find current file
map <Leader>. :NERDTreeFind<CR>

" Saves with CTRL+S or CMD+S
noremap <silent> <C-s> :update<CR>
noremap <silent> <D-s> :update<CR>

" Closes current buffer with <Leader>-w
noremap <Leader>w :bd<CR>

" Disable arrow keys (Vim learner)
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Makes Esc + Esc clear the last search
nnoremap <silent> <Esc><Esc> :let @/=""<CR>
