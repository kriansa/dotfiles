" Disable Nvim remote plugins
let g:loaded_python_provider = 1
let g:loaded_python3_provider = 1
let g:loaded_ruby_provider = 1
let g:loaded_node_provider = 1
let g:loaded_remote_plugins = 1

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
" Disable the list of buffers
let g:airline#extensions#tabline#enabled = 0

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
let g:airline_symbols.notexists = ' '

" Highlight yank
let g:highlightedyank_highlight_duration = 150

" Configure Vim-Ruby plugin
let ruby_fold = 0
let ruby_foldable_groups = 'NONE'
let ruby_spellcheck_strings = 0
let ruby_no_expensive = 1
" Folding in Ruby is expensive
" let ruby_foldable_groups = 'def'
" let g:ruby_operators = 1

" Enable node plugin on the following filetypes
let g:node_filetypes = ["javascript", "json", "jsx", "vue"]

" Enable autosave
let g:auto_save = 1
let g:auto_save_silent = 1

" Ale
let g:ale_change_sign_column_color = 1
let g:ale_sign_column_always = 1
let g:ale_javascript_eslint_executable = 'eslint_d'
" let g:ale_linters = { 'vue': ['stylelint', 'eslint', 'prettier'] }
let g:ale_linter_aliases = { 'vue': ['javascript', 'css'] }

" DEBUG THIS: It's taking too long to lint & save a file
" let g:ale_fix_on_save = 1
" let g:ale_fixers = {
" \   'javascript': ['prettier'],
" \   'vue': ['prettier'],
" \}

" Ferret
let g:FerretGrepCommand='ag --vimgrep --width 4096 --hidden --ignore .git'

" The Silver Searcher
"
" Use ag over grep
set grepprg=ag\ --nogroup\ --nocolor\ --hidden\ --ignore\ .git

" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
let g:ctrlp_user_command = 'ag %s --hidden --ignore .git -l -g ""'
