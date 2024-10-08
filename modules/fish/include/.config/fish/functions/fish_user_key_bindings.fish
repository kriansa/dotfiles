# Toggle the sudo suffix in the CLI with Ctrl-S (this is already built-in on Fish through Alt+S)
bind \cs "fish_commandline_prepend sudo"

# Opens current path with $EDITOR
bind \ce "$EDITOR ."

# Edit the current line in $EDITOR (C-X, C-E)
bind \cx\ce edit_command_buffer
