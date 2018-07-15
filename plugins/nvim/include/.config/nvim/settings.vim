" Vim settings

syntax on             " Enable syntax highlighting
set backspace=indent,eol " Enables backspace on indentation and end of lines
set hidden            " This allows buffers to be hidden if you've modified a buffer.
" set number            " Display line numbers
" set relativenumber    " Display line numbers relative to the one you're in
set nowrap            " Disable word-wrap
set wildmenu          " Helps command-line completion menu
" set scrolloff=3       " Scroll with at least 3 lines
set encoding=utf-8    " Enables utf8 encoding
" set cursorline        " Highlight the line the cursor is in
" set noshowmode        " Disable showing the mode (such as -- INSERT --) in the bottom
set noswapfile        " Never create swap files
set nobackup          " Disable usage of backup files (~)
set nowritebackup     " Disable creation of backup files
set mouse=            " Disables the mouse
set visualbell t_vb=  " Disable bells on errors
set laststatus=2      " Always enable bottom status line (airline)
" set noesckeys         " Disable using esc keys
set hlsearch          " Enable search highlight
set autoread          " Enable auto-read of files edited outside vim
set synmaxcol=200     " Limit syntax highlighting for long lines
set colorcolumn=80    " Set a width to show a column

set list                                  " show hidden chars
set listchars=tab:▸\ ,eol:¬,space:.       " chars to be shown
set showbreak=↪                           " char to be shown on wraped lines

" Clipboard settings
set clipboard+=unnamed                    " yanks to clipboard

" On Linux, yank to clipboard by default, not the PRIMARY (middle mouse btn)
if has("unix") && !has("mac")
  let g:clipboard = {
        \   'name': 'xclip-clipboard',
        \   'copy': {
        \      '+': 'xclip -selection clipboard',
        \      '*': 'xclip -selection clipboard',
        \    },
        \   'paste': {
        \      '+': 'xclip -selection clipboard -o',
        \      '*': 'xclip -selection clipboard -o',
        \   },
        \   'cache_enabled': 1,
        \ }
endif

" Indentation settings
set expandtab           " Convert tabs into spaces
set autoindent          " always set autoindenting on
set copyindent          " copy indentation on new lines
set smartindent         " indent on new blocks
set preserveindent      " When reindenting a line, tries to preserve the indent-style
set shiftwidth=2        " Number of spaces to use for autoindenting
set smarttab            " Insert tabs on the start of a line according to shiftwidth, not tabstop
set shiftround          " Use multiple of shiftwidth when indenting with '<' and '>'

set complete+=kspell    " Use spell completion when spell check is enabled

" Search
set ignorecase   " Make search case insensitive
set smartcase    " When searching with a uppercase letter, enable case-sensitive
set inccommand=split

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Folding settings
" That is incredibly slow
" set foldmethod=syntax
" autocmd BufWinEnter * silent! :%foldopen!

" Disable vim autocompletion for these files below
set wildignore=node_modules,.git,.DS_Store

" Autocommands

" Exit paste when leaving InsertMode
autocmd InsertLeave * set nopaste

" Terminal settings
autocmd TermOpen term://* setlocal nolist
" autocmd BufWinEnter,WinEnter term://* startinsert
" autocmd BufLeave term://* stopinsert

" Temporarily workaround for nvim bug:
" https://github.com/neovim/neovim/issues/7483
" TODO: Remove that when this bug is fixed.
autocmd VimResized * redraw!
