" Configure python environment
"
let g:python_host_prog = systemlist('PYENV_VERSION=2.7.13 pyenv which python2')[0]
let g:python3_host_prog = systemlist('PYENV_VERSION=3.6.1 pyenv which python3')[0]

" NERDTree settings
"
" Enable hidden files on NERDTree
let g:NERDTreeShowHidden=1
" Keep it when we open a file through the tree
let g:NERDTreeQuitOnOpen=0
" Ignore metadata
let g:NERDTreeIgnore=['.git$', '\~$']
" Disable netrw
let loaded_netrwPlugin = 1

" Airline settings
"
" Enable powerline fonts
let g:airline_powerline_fonts = 1
" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 0
" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
" Buffer tabs separators
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#close_symbol = '✖'
let g:airline#extensions#tabline#buffers_label = 'B'
let g:airline#extensions#tabline#tabs_label = 'T'
" Load custom formatter available at:
" autoload/airline/extensions/tabline/formatters/custom.vim
let g:airline#extensions#tabline#formatter = 'custom'

" Enable one-letter status modes
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V-L',
    \ '' : 'V-B',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '' : 'S',
    \ 't'  : 'T',
    \ }

" Turn the status on the right cleaner
let g:airline_skip_empty_sections = 1
let g:airline_section_z = '%#__accent_bold#%{g:airline_symbols.linenr}%4l%#__restore__#%#__accent_bold#/%L%#__restore__# :%3v'
let g:airline_section_y = ''

" Change the 'non-versioned' symbol
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.notexists = ' ✗'

" Highlight yank
let g:highlightedyank_highlight_duration = 300

" Configure Vim-Ruby plugin
"
let ruby_foldable_groups = 'def'
let g:ruby_operators = 1

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" Configure TernJS
"
let g:tern_request_timeout = 1

" Add extra filetypes
let g:tern#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'javascript',
                \ 'vue'
                \ ]

" Disable polyglot languages
let g:polyglot_disabled = ['javascript', 'ruby']

" Enable node plugin on the following filetypes
let g:node_filetypes = ["javascript", "json", "jsx", "vue"]

" Enable autosave
let g:auto_save = 1
let g:auto_save_silent = 1

" Ale
let g:ale_javascript_eslint_executable = 'eslint_d'

" Ferret
let g:FerretGrepCommand='ag --vimgrep --width 4096 --hidden --ignore .git'

" The Silver Searcher
"
" Use ag over grep
set grepprg=ag\ --nogroup\ --nocolor\ --hidden\ --ignore\ .git

" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
let g:ctrlp_user_command = 'ag %s --hidden --ignore .git -l -g ""'
