" Packages
"
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Deoplete (autocomplete for neovim)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'fishbullet/deoplete-ruby'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Languages support
Plug 'sheerun/vim-polyglot'
Plug 'vim-ruby/vim-ruby'

" Ruby
Plug 'tpope/vim-rails'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rake'

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'moll/vim-node'

" CSS
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'

" HTML
Plug 'othree/html5.vim'
Plug 'posva/vim-vue'

" Surrounding (things that should be native)
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'tomtom/tcomment_vim'

" Text objects
Plug 'kana/vim-textobj-user'
Plug 'rhysd/vim-textobj-ruby'

" Emmet
Plug 'mattn/emmet-vim'

" Style
Plug 'ryanoasis/vim-devicons'
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'morhetz/gruvbox'
Plug 'crusoexia/vim-monokai'
Plug 'KabbAmine/yowish.vim'
Plug 'mhartington/oceanic-next'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" EditorConfig support
Plug 'editorconfig/editorconfig-vim'

" Fuzzy finder
Plug 'ctrlpvim/ctrlp.vim'

" Multi-file finder
Plug 'wincent/ferret'
" Promising project far.vim: Find And Replace Vim plugin
" Plug 'brooth/far.vim'

" Indent guides
Plug 'nathanaelkane/vim-indent-guides'

" GnuPG integration
Plug 'jamessan/vim-gnupg'

" Tags support
Plug 'ludovicchabant/vim-gutentags'

" Async support
Plug 'neomake/neomake'

" Think about this Linter
" Plug 'w0rp/ale'

" Add plugins to &runtimepath
call plug#end()
