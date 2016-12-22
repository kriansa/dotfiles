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

" Enables backspace
set backspace=indent,eol,start

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
set nobackup          " Disable creation of backup files (~)
set mouse=            " Disables the mouse
set visualbell t_vb=  " Disable bells on errors

set list                                  " show hidden chars
set listchars=tab:▸\ ,eol:¬,space:.       " chars to be shown
set clipboard+=unnamed                    " yanks to clipboard

set expandtab                " Convert tabs into spaces
set autoindent               " always set autoindenting on
set copyindent               " copy indentation on new lines
set smartindent              " indent on new blocks
set preserveindent           " When reindenting a line, tries to preserve the indent-style

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
" Keep it when opens a file through the tree
let g:NERDTreeQuitOnOpen=0
" Ignore GIT metafiles
let g:NERDTreeIgnore=['.git$', '\~$']
" Configure NERDTree icons
let g:WebDevIconsUnicodeDecorateFolderNodes = 0
let g:DevIconsEnableFoldersOpenClose = 0
" adding the flags to NERDTree
let g:webdevicons_enable_nerdtree = 0
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

" Indent plugin
let g:indent_guides_start_level = 2

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor\ --hidden\ --ignore\ .git

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g "" --ignore .git'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Match different file extensions to types
autocmd BufNewFile,BufRead *.vue set filetype=html
autocmd BufNewFile,BufRead *.babelrc set filetype=javascript

" Adds a layer on top of Obsession plugin so we have a structure
" that saves every session into the project folder in a file
" named `.session.vim`

autocmd VimEnter * :call LoadSession()
function! LoadSession()
  if (isdirectory(argv(0)))
    let file = getcwd() . '/.session.vim'
    if (filereadable(file))
      exe 'source ' . file
      echo 'Session loaded'
    endif
  endif
endfunction

command! SaveSession call SaveSession()
function! SaveSession()
  if (isdirectory(argv(0)))
    execute ":Obsession" . getcwd() . '/.session.vim'
  else
    echo "Can't save a session from a single file. Open vim using a directory as an argument!"
  endif
endfunction

" Enable match-it (use % to jump to a matching tag, such as if and endif)
runtime 'macros/matchit.vim'

" Theme settings
"

" Enable true colors
set termguicolors

"set background=dark
"let g:gruvbox_bold = 0
"colorscheme gruvbox

" Monokai
colorscheme monokai
highlight NonText guifg = #444444
highlight SpecialKey guifg = #444444

" Quantum template
"colorscheme quantum
"let g:airline_theme = 'quantum'
"highlight NonText guifg = #414c52
"highlight SpecialKey guifg = #414c52
