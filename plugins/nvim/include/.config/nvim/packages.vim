" Packages
"
call plug#begin('~/.config/nvim/plugged')

" Nerdtree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" Vim Airline
Plug 'vim-airline/vim-airline'

" Multiple languages
Plug 'sheerun/vim-polyglot'

" Deoplete (autocomplete for neovim)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Obsession (project/session manager)
Plug 'tpope/vim-obsession'

" Style
Plug 'ryanoasis/vim-devicons'
Plug 'tyrannicaltoucan/vim-quantum'

" Languages support
Plug 'vim-ruby/vim-ruby'
Plug 'posva/vim-vue'
Plug 'pangloss/vim-javascript'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" EditorConfig support
Plug 'editorconfig/editorconfig-vim'

" Command T functionality
Plug 'wincent/command-t', { 'do': 'export RBENV_VERSION=2.3.3 && cd ruby/command-t && ruby extconf.rb && make' }

" Add plugins to &runtimepath
call plug#end()
