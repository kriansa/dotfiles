" Load packages
runtime packages.vim

" Load shortcuts
runtime shortcuts.vim

" Load an IDE-like environment for vim
runtime ide.vim

" Settings
"

" Enables backspace on indentation and end of lines
set backspace=indent,eol

" VIM Settings
syntax on             " Enable syntax highlighting
set hidden            " This allows buffers to be hidden if you've modified a buffer.
set number            " Display line numbers
set relativenumber    " Display line numbers relative to the one you're in
set nowrap            " Disable word-wrap
set wildmenu          " Helps command-line completion menu
set scrolloff=3       " Scroll with at least 3 lines
set encoding=utf-8    " Enables utf8 encoding
set cursorline        " Highlight the line the cursor is in
set noshowmode        " Disable showing the mode (such as -- INSERT --) in the bottom
set noswapfile        " Never create swap files
set nobackup          " Disable usage of backup files (~)
set nowritebackup     " Disable creation of backup files
set mouse=            " Disables the mouse
set visualbell t_vb=  " Disable bells on errors

set list                                  " show hidden chars
set listchars=tab:▸\ ,eol:¬,space:.       " chars to be shown
set clipboard+=unnamed                    " yanks to clipboard

set expandtab           " Convert tabs into spaces
set autoindent          " always set autoindenting on
set copyindent          " copy indentation on new lines
set smartindent         " indent on new blocks
set preserveindent      " When reindenting a line, tries to preserve the indent-style
set shiftwidth=2        " Number of spaces to use for autoindenting
set smarttab            " Insert tabs on the start of a line according to shiftwidth, not tabstop
set shiftround          " Use multiple of shiftwidth when indenting with '<' and '>'

set ignorecase   " Make search case insensitive
set smartcase    " When searching with a uppercase letter, enable case-sensitive

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Disable vim autocompletion for these files below
set wildignore=node_modules,.git,.DS_Store

" Theme settings
"
" Enable true colors
set termguicolors

" Monokai
colorscheme monokai
highlight NonText guifg = #444444
highlight SpecialKey guifg = #444444

" Quantum template
" colorscheme quantum
" let g:airline_theme = 'quantum'
" highlight NonText guifg = #414c52
" highlight SpecialKey guifg = #414c52

" Enable match-it (use % to jump to a matching tag, such as if and endif)
runtime macros/matchit.vim

" Configure python environment
" 
let g:python_host_prog = systemlist('PYENV_VERSION=2.7.12 pyenv which python2')[0]
let g:python3_host_prog = systemlist('PYENV_VERSION=3.5.2 pyenv which python3')[0]

" Plugin settings
"
" Enable hidden files on NERDTree
let g:NERDTreeShowHidden=1
" Keep it when we open a file through the tree
let g:NERDTreeQuitOnOpen=0
" Ignore metadata
let g:NERDTreeIgnore=['.git$', '\~$']
" Disable DevIcons for NERDTree
let g:WebDevIcons_enable_nerdtree = 0
let g:WebDevIconsUnicodeDecorateFolderNodes = 0
let g:DevIconsEnableFoldersOpenClose = 0
" Disable netrw
let loaded_netrwPlugin = 1

" Airline settings
"
" Enable powerline fonts
let g:airline_powerline_fonts = 1
" Integrate with Obsession
let g:airline#extensions#obsession#enabled = 1
" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
" Buffer tabs separators
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline#extensions#tabline#close_symbol = '✖'

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" Disable polyglot languages
let g:polyglot_disabled = ['javascript', 'ruby']

" Enable node plugin on the following filetypes
let g:node_filetypes = ["javascript", "json", "jsx", "vue"]

" Indent plugin
let g:indent_guides_start_level = 2

" Enable autosave
let g:auto_save = 1
let g:auto_save_silent = 1

" The Silver Searcher
"
" Use ag over grep
set grepprg=ag\ --nogroup\ --nocolor\ --hidden\ --ignore\ .git

" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
let g:ctrlp_user_command = 'ag %s -l --max-count 1 --hidden --nocolor -g "" --ignore .git'
