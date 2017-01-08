" IDE metadata folder
let g:ide_metadata_dir = '.git'

" Gutentags configs
"
let g:gutentags_tagfile = g:ide_metadata_dir . '/tags'
let g:gutentags_ctags_executable = 'vim-tags'
" TODO: There is still something to be added here: auto generation of tags for
" installed gems, ruby stdlib and npm packages

" Adds a layer on top of Obsession plugin so we have a structure
" that saves every session into the project folder in a file
" named `.git/session.vim`

autocmd VimEnter * nested LoadSession
command! LoadSession call LoadSession()
function! LoadSession()
  let file = getcwd() . '/' . g:ide_metadata_dir . '/session.vim'

  if (filereadable(file))
    execute 'source ' . file

    if (bufnr('$') > 1)
      " Sometimes when you open a directory, vim creates a buffer named
      " "~/Project/etc" instead using the full path as the buffer name.
      " So if you want to detect if there is an open buffer for your project
      " folder, you have to check for both absolute path and the home-relative
      " path.
      let explorer_buffer_tilde = bufnr(fnamemodify(getcwd(), ':~'))
      let explorer_buffer_absolute = bufnr(getcwd())

      if (explorer_buffer_tilde != -1)
        execute 'bd ' . explorer_buffer_tilde
      elseif (explorer_buffer_absolute != -1)
        execute 'bd ' . explorer_buffer_absolute
      endif
    endif

    echo 'Session loaded'
  endif
endfunction

command! SaveSession call SaveSession()
function! SaveSession()
  let meta_dir = getcwd() . '/' . g:ide_metadata_dir

  if (!isdirectory(meta_dir))
    echo "This is not a git repository!"
  else
    execute ":Obsession " . meta_dir . '/session.vim'
  endif
endfunction
