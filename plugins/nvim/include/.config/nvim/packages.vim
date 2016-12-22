" Packages
"
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" NerdCommenter
Plug 'scrooloose/nerdcommenter'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Surrounding
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'

" Deoplete (autocomplete for neovim)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'fishbullet/deoplete-ruby'

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Languages support
Plug 'sheerun/vim-polyglot'
Plug 'vim-ruby/vim-ruby'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rake'

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

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" EditorConfig support
Plug 'editorconfig/editorconfig-vim'

" Fuzzy finder
Plug 'ctrlpvim/ctrlp.vim'

" Multi-file finder
Plug 'wincent/ferret'

" Indent guides
Plug 'nathanaelkane/vim-indent-guides'

" GnuPG integration
Plug 'jamessan/vim-gnupg'

" Add plugins to &runtimepath
call plug#end()
