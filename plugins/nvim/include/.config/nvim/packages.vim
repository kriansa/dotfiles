" Packages
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Ruby
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'tpope/vim-bundler'
" Plug 'tpope/vim-rake'
" Plug 'tpope/vim-rails'

" Elixir
Plug 'elixir-lang/vim-elixir'

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'moll/vim-node'

" CSS
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'

" HTML
Plug 'othree/html5.vim'
Plug 'posva/vim-vue', { 'for': 'vue' }

" Languages support
Plug 'sheerun/vim-polyglot'

" Editor features
Plug 'w0rp/ale'
Plug '907th/vim-auto-save'

" Enable preview of registers
Plug 'junegunn/vim-peekaboo'

" Deoplete (autocomplete for neovim)
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'fishbullet/deoplete-ruby'
" Plug 'carlitux/deoplete-ternjs'

" Surrounding and things that should be native
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'jiangmiao/auto-pairs'
Plug 'tomtom/tcomment_vim'
Plug 'justinmk/vim-sneak'
Plug 'machakann/vim-highlightedyank'

" Text objects
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'sgur/vim-textobj-parameter'

" Editor powerups
Plug 'mattn/emmet-vim'
Plug 'editorconfig/editorconfig-vim'

" Style
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'crusoexia/vim-monokai'
Plug 'KabbAmine/yowish.vim'
Plug 'rakr/vim-one'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'cohama/agit.vim'

" Fuzzy finder
Plug 'ctrlpvim/ctrlp.vim'
Plug 'kriansa/ferret', { 'branch': 'feature/ferret-grep-command' }

" Tags support
Plug 'ludovicchabant/vim-gutentags'

" Window management
Plug 'qpkorr/vim-bufkill'
Plug 'simeji/winresizer'
Plug 'troydm/zoomwintab.vim'

" Add plugins to &runtimepath
call plug#end()
