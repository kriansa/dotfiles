" Folder in which script resides
let s:path = expand('<sfile>:p:h')

" Workaround to make Ruby plugin work with a specific Ruby version
" Currently, for Python plugin, we can just set g:python_host_prog
" and it allows you to specify any version you want to load. However
" for ruby, you're stuck with whatever `neovim-ruby-host` binary
" that your current $PATH provides you. So this hack will add a
" new folder to $PATH which contains a `neovim-ruby-host` that can
" then point to any other binary file you want. I choose to make it
" work with a specific rbenv version. Take a look to the script and
" see exactly what it does.
"
let $PATH = s:path . '/bin:' . $PATH

" Load packages
execute 'source ' . s:path . '/packages.vim'

" Load shortcuts
execute 'source ' . s:path . '/shortcuts.vim'

" Settings
"

" Disables backspace (Vim learner)
set backspace=0

" VIM Settings
syntax on             " Enable syntax highlighting
set hidden            " This allows buffers to be hidden if you've modified a buffer.
set number            " Display line numbers
set relativenumber    " Display line numbers relative to the one you're in
set nowrap            " Disable word-wrap
set wildmenu          " Helps command-line completion menu
set scrolloff=3       " Scroll with at least 3 lines
set encoding=utf-8    " Enables utf8 encoding

set list                                  " show hidden chars
set listchars=tab:▸\ ,eol:¬,space:.       " chars to be shown
set clipboard+=unnamed                    " yanks to clipboard

set autoindent               " always set autoindenting on
set copyindent               " copy indentation on new lines
set smartindent              " indent on new blocks

set ignorecase   " Make search case insensitive
set smartcase    " When searching with a uppercase letter, enable case-sensitive

set wildignore=node_modules/**,.git/**,.DS_Store

" Configure python environment
let g:python_host_prog = systemlist('PYENV_VERSION=2.7.12 pyenv which python2')[0]
let g:python3_host_prog = systemlist('PYENV_VERSION=3.5.2 pyenv which python3')[0]

" Plugin settings
"

" Enable hidden files on NERDTree
let g:NERDTreeShowHidden=1
" Enable single click to open/close nodes
let g:NERDTreeMouseMode=2
" Closes it when opens a file through the tree
let g:NERDTreeQuitOnOpen=1
" Ignore GIT metafiles
let g:NERDTreeIgnore=['.git', '\~$']

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

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" Set theme
"
set background=dark
colorscheme quantum
let g:airline_theme = 'quantum'

if has("termguicolors")
  set termguicolors
endif

" Invisible character colors for quantum template
highlight NonText guifg = #414c52
highlight SpecialKey guifg = #414c52
