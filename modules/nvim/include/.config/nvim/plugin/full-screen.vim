" Toggle the full screen mode
function! ToggleFullScreen()
  execute ":ZoomWinTabToggle"
  execute ':AirlineRefresh'
endfunction

" Show a full screen on the status line
function! ShowFullScreenStatus(...)
  " first variable is the statusline builder
  let builder = a:1
  let s:spc = g:airline_symbols.space

  " WARNING: the API for the builder is not finalized and may change
  if gettabvar(tabpagenr(),'zoomwintab') != ''
    call builder.add_section('Tag', s:spc.airline#section#create('⮻ ').s:spc)
  endif

  return 0
endfunction

call airline#add_statusline_func('ShowFullScreenStatus')
