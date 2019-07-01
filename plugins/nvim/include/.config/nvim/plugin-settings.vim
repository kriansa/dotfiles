" External plugins
let g:ruby_host_prog = systemlist("RBENV_VERSION=$(rbenv global) rbenv which neovim-ruby-host")[0]

" NERDTree settings
"
let g:NERDTreeShowHidden=1
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeIgnore=['^\.git$', '\~$']
let g:NERDTreeHijackNetrw=1
let g:NERDTreeMinimalUI=1
let g:NERDTreeAutoDeleteBuffer=1
let g:NERDTreeNaturalSort=1
let g:NERDTreeStatusline="%{exists('b:NERDTree')?fnamemodify(expand(b:NERDTree.root.path.str()), ':~:.'):'NERD'}"
" Disable C-J, C-K mappings
let g:NERDTreeMapJumpPrevSibling=""
let g:NERDTreeMapJumpNextSibling=""

" Disable netrw
let loaded_netrwPlugin = 1

" Configure CommandT
let g:CommandTFileScanner="git"
let g:CommandTGitScanSubmodules=1
let g:CommandTGitIncludeUntracked=1
let g:CommandTScanDotDirectories=1
let g:CommandTAlwaysShowDotFiles=1

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
" let g:airline_section_z = '%#__accent_bold#%{g:airline_symbols.linenr}%4l%#__restore__#%#__accent_bold#/%L%#__restore__# :%3v'
let g:airline_section_z = ''
let g:airline_section_y = ''

" Change the 'non-versioned' symbol
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.notexists = ' '

" Highlight yank
let g:highlightedyank_highlight_duration = 80

" Configure Vim-Ruby plugin
let ruby_foldable_groups = 'NONE'
let ruby_spellcheck_strings = 0
" Folding in Ruby is expensive
let ruby_fold = 0

" Enable node plugin on the following filetypes
let g:node_filetypes = ["javascript", "json", "jsx", "vue"]

" Ale
let g:ale_change_sign_column_color = 1
let g:ale_sign_column_always = 1
let g:ale_javascript_eslint_executable = 'eslint_d'
let g:ale_fixers = {
\   'javascript': ['prettier'],
\   'vue': ['prettier'],
\   'ruby': ['rubocop']
\}

" Use deoplete.
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources = { '_': ['ale'] }

" Ferret
let g:FerretExecutableArguments = {
\  'ag': '--vimgrep --width 4096 --hidden --ignore .git'
\}

" The Silver Searcher
"
" Use ag over grep
set grepprg=ag\ --nogroup\ --nocolor\ --hidden\ --ignore\ .git

" Matchup settings
"
" Allow a small delay when highlighting matches
let g:matchup_matchparen_deferred = 1

" Disable replacing the statusline by the matching delimiter if not on screen
let g:matchup_matchparen_status_offscreen = 0
