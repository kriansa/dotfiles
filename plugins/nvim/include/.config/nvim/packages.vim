" Packages
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree', { 'commit': '67fa9b3116948466234978aa6287649f98e666bd' }

" Vim Airline
Plug 'vim-airline/vim-airline'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Syntax support
Plug 'pearofducks/ansible-vim', { 'for': 'ansible' }
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
Plug 'tbastos/vim-lua', { 'for': 'lua' }
Plug 'keith/swift.vim', { 'for': 'swift' }
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }

" Markdown
" Plug 'SidOfc/mkdx'
Plug 'junegunn/vim-easy-align'

" Ruby
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-bundler'

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'moll/vim-node'

" CSS
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'ap/vim-css-color'

" HTML
Plug 'othree/html5.vim'
Plug 'posva/vim-vue'

" Editor features
Plug 'w0rp/ale'

" Surrounding and things that should be native
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'jiangmiao/auto-pairs'
Plug 'tomtom/tcomment_vim'
Plug 'justinmk/vim-sneak'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'andymass/vim-matchup'

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
Plug 'wincent/ferret'
Plug 'brooth/far.vim'
Plug 'wincent/command-t', { 'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make' }

" Tags support
Plug 'ludovicchabant/vim-gutentags'

" Window management
Plug 'qpkorr/vim-bufkill'
Plug 'simeji/winresizer'
Plug 'troydm/zoomwintab.vim'

" Highlighted yank
Plug 'machakann/vim-highlightedyank'

" Add plugins to &runtimepath
call plug#end()
