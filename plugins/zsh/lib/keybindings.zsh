# Key bindings
# ============

# This is the list of characters that ZSH consider to be a word. It's useful
# when you want to move between words using 'backward-word' and 'forward-word'
export WORDCHARS='*?_[]~=&;!#$%^(){}'

# Use emacs keybindings
bindkey -e

# Configure the key bindings for using history
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward
bindkey -M vicmd 'k' up-line-or-beginning-search
bindkey -M vicmd 'j' down-line-or-beginning-search

# Use tab to choose between the suggestion list on history widget
bindkey -M menuselect '^o' accept-and-infer-next-history

# Use tab to autocomplete using "..." when loading the completion
bindkey "^I" expand-or-complete-with-dots # [Tab]

# Do history expansion
bindkey ' ' magic-space # [Space]

# Move through the completion menu backwards
bindkey "${terminfo[kcbt]}" reverse-menu-complete # [Shift-Tab]

# Forward-delete a character
bindkey "${terminfo[kdch1]}" delete-char # [Delete] - delete forward

# Home and end
bindkey "^[[H" beginning-of-line # [Home] - begining of line
bindkey "^[[F" end-of-line # [End] - end of line

# CTRL-P cuts the line
bindkey "^P" _cut-whole-line

# This will only work on linux
# bindkey '^[[1;5C' forward-word # [Ctrl-RightArrow] - move forward one word
# bindkey '^[[1;5D' backward-word # [Ctrl-LeftArrow] - move backward one word
