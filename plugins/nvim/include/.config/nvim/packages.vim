" Packages
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'preservim/nerdtree'
Plug 'justinmk/vim-dirvish'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-projectionist'

" Syntax support
Plug 'pearofducks/ansible-vim', { 'for': 'ansible' }
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
Plug 'tbastos/vim-lua', { 'for': 'lua' }
Plug 'keith/swift.vim', { 'for': 'swift' }
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'jakwings/vim-pony', { 'for': 'pony' }
Plug 'slim-template/vim-slim', { 'for': 'slim' }
Plug 'jvirtanen/vim-hcl', { 'for': 'hcl' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'HerringtonDarkholme/yats.vim', { 'for': 'typescript' }
Plug 'pantharshit00/vim-prisma', { 'for': 'prisma' }

" Markdown
" Plug 'SidOfc/mkdx'
Plug 'junegunn/vim-easy-align'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }

" Ruby
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'moll/vim-node'
Plug 'maxmellon/vim-jsx-pretty'

" CSS
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'ap/vim-css-color'

" HTML
Plug 'othree/html5.vim'
Plug 'posva/vim-vue'

" Editor features
Plug 'w0rp/ale'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'neoclide/coc-tsserver', { 'do': 'yarn install --frozen-lockfile' }
Plug 'neoclide/coc-css', { 'do': 'yarn install --frozen-lockfile' }
Plug 'neoclide/coc-html', { 'do': 'yarn install --frozen-lockfile' }
Plug 'neoclide/coc-json', { 'do': 'yarn install --frozen-lockfile' }
Plug 'neoclide/coc-yaml', { 'do': 'yarn install --frozen-lockfile' }
Plug 'neoclide/coc-vetur', { 'do': 'yarn install --frozen-lockfile' }
Plug 'neoclide/coc-solargraph', { 'do': 'yarn install --frozen-lockfile' }
Plug 'josa42/coc-go', { 'do': 'yarn install --frozen-lockfile' }

" Surrounding and things that should be native
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'jiangmiao/auto-pairs'
Plug 'tomtom/tcomment_vim'
Plug 'justinmk/vim-sneak'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'andymass/vim-matchup'

" Adds new useful unix commands
Plug 'tpope/vim-eunuch'

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
Plug 'NLKNguyen/papercolor-theme'
Plug 'rakr/vim-two-firewatch'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'cohama/agit.vim'

" Fuzzy finder
Plug 'wincent/ferret'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'brooth/far.vim'

" Window management
Plug 'qpkorr/vim-bufkill'
Plug 'simeji/winresizer'
Plug 'troydm/zoomwintab.vim'
Plug 'junegunn/goyo.vim'

" Highlighted yank
Plug 'machakann/vim-highlightedyank'

" Add plugins to &runtimepath
call plug#end()
