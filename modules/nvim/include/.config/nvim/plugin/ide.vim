" IDE metadata folder
let g:ide_metadata_dir = '.git'

" Adds a layer on top of Obsession plugin so we have a structure
" that saves every session into the project folder in a file
" named `.git/session.vim`

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
  endif
endfunction

command! SaveSession call SaveSession()
function! SaveSession()
  let meta_dir = getcwd() . '/' . g:ide_metadata_dir

  if (!isdirectory(meta_dir))
    echo "This directory doesn't have a metadir (" . meta_dir . ") !"
  else
    execute ":Obsession " . meta_dir . '/session.vim'
  endif
endfunction

" Enable session to be automatically loaded if you specify nvim to be opened
" in a directory instead a single file
autocmd VimEnter * nested if (isdirectory(argv(0))) | execute 'cd ' . argv(0) | call LoadSession() | endif
