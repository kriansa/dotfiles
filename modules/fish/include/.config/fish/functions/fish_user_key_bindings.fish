# Toggle the sudo suffix in the CLI with Ctrl-S (this is already built-in on Fish through Alt+S)
bind ctrl-s "fish_commandline_prepend sudo"

# Opens current path with $EDITOR
bind ctrl-shift-e "$EDITOR ."

# Edit the current line in $EDITOR (C-X, C-E)
bind ctrl-x,ctrl-e edit_command_buffer
