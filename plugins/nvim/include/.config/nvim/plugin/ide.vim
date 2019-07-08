    echo "This directory doesn't have a metadir (" . meta_dir . ") !"
  else
    execute ":Obsession " . meta_dir . '/session.vim'
  endif
endfunction

" Enable session to be automatically loaded if you specify nvim to be opened
" in a directory instead a single file
autocmd VimEnter * nested if (isdirectory(argv(0))) | execute 'cd ' . argv(0) | call LoadSession() | endif
