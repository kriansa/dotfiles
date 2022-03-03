" Disable all external plugin integrations
let g:loaded_python_provider = 0
let g:loaded_python3_provider = 0
let g:loaded_ruby_provider = 0
let g:loaded_perl_provider = 0
let g:loaded_node_provider = 0

" NERDTree settings
let g:NERDTreeShowHidden=1
let g:NERDTreeQuitOnOpen=0
let g:NERDTreeSortHiddenFirst=1
let g:NERDTreeIgnore=['^\.git$', '\~$']
let g:NERDTreeRespectWildIgnore=1
let g:NERDTreeHijackNetrw=1
let g:NERDTreeMinimalUI=1
let g:NERDTreeAutoDeleteBuffer=1
let g:NERDTreeNaturalSort=1
let g:NERDTreeStatusline="%{exists('b:NERDTree')?fnamemodify(expand(b:NERDTree.root.path.str()), ':~:.'):'NERD'}"

" Disable deprecated commands on fugitive
let fugitive_legacy_commands = 0

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
" let g:airline_section_z = '%#__accent_bold#%{g:airline_symbols.linenr}%4l%#__restore__#%#__accent_bold#/%L%#__restore__# :%3v'
let g:airline_section_z = ''
let g:airline_section_y = ''

" Change the 'non-versioned' symbol
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.notexists = ' [new]'

" Highlight yank
let g:highlightedyank_highlight_duration = 80

" Configure Vim-Ruby plugin
let ruby_fold = 0
let ruby_foldable_groups = 'NONE'
let ruby_spellcheck_strings = 0
let g:ruby_indent_assignment_style = 'variable'
let g:ruby_indent_hanging_elements = 0
" Folding in Ruby is expensive
let ruby_fold = 0

" Enable node plugin on the following filetypes
let g:node_filetypes = ["javascript", "json", "jsx", "vue"]

" Ale
let g:ale_disable_lsp = 1 " Disable LSP so we let CoC handle it
let g:ale_change_sign_column_color = 1
let g:ale_sign_column_always = 1
let g:ale_linters = {'ruby': ['standardrb']}
let g:ale_fixers = {
\  '*': ['remove_trailing_lines', 'trim_whitespace'],
\  'javascript': ['prettier'],
\  'typescript': ['prettier'],
\  'vue': ['prettier'],
\  'ruby': ['standardrb'],
\  'tf': ['terraform'],
\  'markdown': ['remark-lint'],
\  'python': ['black']
\}

" Editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
let g:EditorConfig_preserve_formatoptions = 1

" Ferret
let g:FerretExecutableArguments = {
\  'rg': '--vimgrep -M 120 --hidden --ignore-file=' . $DOTFILES_PATH . '/.rgignore'
\}

" Ripgrep
"
" Use rg over grep
set grepprg=rg\ --no-heading\ -M\ 120\ --color=never\ --hidden\ --ignore-file=$DOTFILES_PATH/.rgignore

" Matchup settings
"
" Allow a small delay when highlighting matches
let g:matchup_matchparen_deferred = 1

" Disable replacing the statusline by the matching delimiter if not on screen
let g:matchup_matchparen_status_offscreen = 0

" Set defaults for Goyo
let g:goyo_linenr = 1
let g:goyo_width = "60%"
let g:goyo_height = "100%"

" Settings for fzf
let g:fzf_layout = { 'window': {
                \ 'width': 0.9,
                \ 'height': 0.7,
                \ 'highlight': 'Comment',
                \ 'rounded': v:false } }

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

" Configure fzf
let $FZF_DEFAULT_OPTS = "--bind=ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-b:page-up,ctrl-f:page-down"
let $FZF_DEFAULT_COMMAND = 'rg --hidden --ignore-file=' . $DOTFILES_PATH . '/.rgignore --files -g ""'

" My own autosave (plugins/write-files.vim)
let g:auto_save = 1

" Git gutter
autocmd BufWritePost,TextChanged,TextChangedI * GitGutter
