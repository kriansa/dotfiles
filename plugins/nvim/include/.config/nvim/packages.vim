" Packages
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Ansible
Plug 'pearofducks/ansible-vim', { 'for': 'ansible' }

" Terraform
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }

" Clojure
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }

" Crystal
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }

" Lua
Plug 'tbastos/vim-lua', { 'for': 'lua' }

" Swift
Plug 'keith/swift.vim', { 'for': 'swift' }

" CSS color viewer
Plug 'ap/vim-css-color'

" Markdown
" Plug 'SidOfc/mkdx'
Plug 'junegunn/vim-easy-align'

" Ruby
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-bundler'

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
Plug 'posva/vim-vue'

" Editor features
Plug 'w0rp/ale'
Plug '907th/vim-auto-save'

" Surrounding and things that should be native
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'jiangmiao/auto-pairs'
Plug 'tomtom/tcomment_vim'
Plug 'justinmk/vim-sneak'

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
Plug 'wincent/ferret'

" Tags support
Plug 'ludovicchabant/vim-gutentags'

" Window management
Plug 'qpkorr/vim-bufkill'
Plug 'simeji/winresizer'
Plug 'troydm/zoomwintab.vim'

" Open file using file manager (gof & got)
Plug 'justinmk/vim-gtfo'

" Highlighted yank
Plug 'machakann/vim-highlightedyank'

" Add plugins to &runtimepath
call plug#end()
