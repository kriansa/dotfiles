" This function pastes in visual mode without yanking the previous content
"
" Extracted from: https://superuser.com/a/1055424
function! SmartPaste()
    let mode = 'gv'

    let delete = '"_d'

    let reg = '"+'

    let currentLine = line(".")
    let lastLineOfBuffer = line("$")
    let selectionEndLine = line("'>")
    let selectionEndLineLength = len(getline(selectionEndLine))
    let nextLineLength = len(getline(currentLine + 1))
    let selectionEndColumn = col("'>")

    " If selection does not include or go beyond the last visible character of the line (by also selecting the invisible EOL character)
    if selectionEndColumn < selectionEndLineLength
        let cmd = 'P'

    " If attempting to paste on a blank last line
    elseif selectionEndLineLength == 0 && selectionEndLine == lastLineOfBuffer
        let cmd = 'P'

    " If selection ends after the last visible character of the line (by also selecting the invisible EOL character) and next line is not blank and not the last line
    elseif selectionEndColumn > selectionEndLineLength && nextLineLength > 0 && selectionEndLine != lastLineOfBuffer
        let cmd = 'P'

    " If selection ends after the last visible character of the line (by also selecting the invisible EOL character), or the line is visually selected (Shift + V), and next line is the last line
    elseif selectionEndColumn > selectionEndLineLength && selectionEndLine == lastLineOfBuffer
        let cmd = 'p'
        call append(selectionEndLine, "")

    else
        let cmd = 'p'
    endif

    if visualmode() != "\<C-v>" " If not Visual-Block mode
        " Trim the last \r\n | \n | \r character in the '+' buffer
        " Note: This messes up Visual-Block pasting.
        let @+ = substitute(@+,'\(\r\?\n\|\r\)$','','g')
    endif

    try
        execute 'normal! ' . mode . delete . reg . cmd
    catch /E353:\ Nothing\ in\ register\ +/
    endtry

    " Move caret one position to right
    call cursor(0, col(".") + 1)
endfunction
