" Theme settings
"
" Enable true colors
set termguicolors

" This method fixes a few color schemes for specific colors that
" don't look well for invisible characters (such as spaces and CRs)
"
autocmd ColorScheme * call FixColorScheme()
function! FixColorScheme()
  if (g:colors_name == "quantum")
    if (g:quantum_black == 1)
      " Fix for quantum black
      highlight NonText guifg = #333333
      highlight SpecialKey guifg = #333333
    else
      " Fix for quantum blue
      highlight NonText guifg = #414c52
      highlight SpecialKey guifg = #414c52
    endif
  elseif (g:colors_name == "monokai")
    " Fix for monokai
    highlight NonText guifg = #444444
    highlight SpecialKey guifg = #444444
  endif
endfunction

" Select the theme
"
" Monokai
" color monokai

" Quantum template
" let g:quantum_black=1
" color quantum

" Yowish
" color yowish

" One
color one-light
