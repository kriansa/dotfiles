" Packages
"
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Ruby
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-bundler'
" Plug 'tpope/vim-rake'
" Plug 'tpope/vim-rails'

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'moll/vim-node'

" CSS
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'

" HTML
Plug 'othree/html5.vim'
Plug 'posva/vim-vue'

" Languages support
Plug 'sheerun/vim-polyglot'

" Deoplete (autocomplete for neovim)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'fishbullet/deoplete-ruby'
Plug 'carlitux/deoplete-ternjs'

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
Plug 'rhysd/vim-textobj-anyblock'
Plug 'sgur/vim-textobj-parameter'

" Emmet
Plug 'mattn/emmet-vim'

" Style
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'crusoexia/vim-monokai'
Plug 'KabbAmine/yowish.vim'
Plug 'rakr/vim-one'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'cohama/agit.vim'

" EditorConfig support
Plug 'editorconfig/editorconfig-vim'

" Fuzzy finder
Plug 'ctrlpvim/ctrlp.vim'

" Multi-file finder
Plug 'wincent/ferret'
" Promising project far.vim: Find And Replace Vim plugin
" Plug 'brooth/far.vim'
" Plug 'dyng/ctrlsf.vim'

" Tags support
Plug 'ludovicchabant/vim-gutentags'

" Autosave
Plug '907th/vim-auto-save'

" Linter
Plug 'w0rp/ale'

" Add plugins to &runtimepath
call plug#end()
