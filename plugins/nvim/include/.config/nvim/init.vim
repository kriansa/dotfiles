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
" set cursorline        " Highlight the line the cursor is in
" set noshowmode        " Disable showing the mode (such as -- INSERT --) in the bottom
set noswapfile        " Never create swap files
set nobackup          " Disable usage of backup files (~)
set nowritebackup     " Disable creation of backup files
set mouse=            " Disables the mouse
set visualbell t_vb=  " Disable bells on errors
set laststatus=2      " Always enable bottom status line (airline)
set noesckeys         " Disable using esc keys
set hlsearch          " Enable search highlight
set autoread          " Enable auto-read of files edited outside vim
set synmaxcol=200     " Limit syntax highlighting for long lines

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

set complete+=kspell    " Use spell completion when spell check is enabled

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

" Enable different cursor for edit mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=2

" Monokai
" colorscheme monokai
" highlight NonText guifg = #444444
" highlight SpecialKey guifg = #444444

" Quantum template
" colorscheme quantum
" let g:airline_theme = 'quantum'
" highlight NonText guifg = #414c52
" highlight SpecialKey guifg = #414c52

" Yowish
color yowish

" Solarized
" set background=light
" colorscheme solarized8_light

" Enable match-it (use % to jump to a matching tag, such as if and endif)
runtime macros/matchit.vim

" Configure python environment
"
let g:python_host_prog = systemlist('PYENV_VERSION=2.7.13 pyenv which python2')[0]
let g:python3_host_prog = systemlist('PYENV_VERSION=3.6.1 pyenv which python3')[0]

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
"  Disable invisible chars for NERDTree
autocmd FileType nerdtree setlocal nolist

" Terminal settings
autocmd TermOpen term://* setlocal nolist

" Airline settings
"
" Enable powerline fonts
let g:airline_powerline_fonts = 1
" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
" Buffer tabs separators
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#close_symbol = '✖'
let g:airline#extensions#tabline#buffers_label = 'B'
let g:airline#extensions#tabline#tabs_label = 'T'
" Load custom formatter available at:
" autoload/airline/extensions/tabline/formatters/custom.vim
let g:airline#extensions#tabline#formatter = 'custom'

" Enable one-letter status modes
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V',
    \ '' : 'V',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '' : 'S',
    \ 't'  : 'T',
    \ }

" Turn the status on the right cleaner
let g:airline_skip_empty_sections = 1
let g:airline_section_z = '%#__accent_bold#%{g:airline_symbols.linenr}%4l%#__restore__#%#__accent_bold#/%L%#__restore__# :%3v'
let g:airline_section_y = ''

" Change the "non-versioned" symbol
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.notexists = ' ✗'

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" Use ternjs
let g:tern_request_timeout = 1

" Add extra filetypes
let g:tern#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'javascript',
                \ 'vue'
                \ ]

" Disable polyglot languages
let g:polyglot_disabled = ['javascript', 'ruby']

" Enable node plugin on the following filetypes
let g:node_filetypes = ["javascript", "json", "jsx", "vue"]

" Indent plugin
let g:indent_guides_start_level = 2

" Enable autosave
let g:auto_save = 1
let g:auto_save_silent = 1

" Ale
let g:ale_javascript_eslint_executable = 'eslint_d'

" Enable spell checking for git commit messages
autocmd FileType gitcommit setlocal spell

" The Silver Searcher
"
" Use ag over grep
set grepprg=ag\ --nogroup\ --nocolor\ --hidden\ --ignore\ .git

" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
let g:ctrlp_user_command = 'ag %s --hidden --ignore .git -l -g ""'
