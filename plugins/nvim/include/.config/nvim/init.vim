call plug#begin('~/.config/.nvim/plugged')

" Make sure you use single quotes

" Nerdtree
Plug 'scrooloose/nerdtree'

" NerdTREE Tabs
Plug 'jistr/vim-nerdtree-tabs'

" Vim-Ruby
Plug 'vim-ruby/vim-ruby'

" Fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Solarized color scheme
Plug 'https://github.com/altercation/vim-colors-solarized.git'

" Add plugins to &runtimepath
call plug#end()

" Configs
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

" Disable arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" NerdTREE as Ctrl + \
map <C-\> :NERDTreeTabsToggle<CR>

" Autoclose NerdTREE when it's the last open tab
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Settings
syntax on
set number
set nowrap
set wildmenu
set relativenumber
set backspace=0

" Solarized theme
set background=dark
colorscheme solarized
