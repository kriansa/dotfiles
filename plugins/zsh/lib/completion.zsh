# Completion settings
# ===================
#
# See ZSH options: http://zsh.sourceforge.net/Doc/Release/Options.html#Options

zmodload zsh/complist

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on successive tab press
setopt complete_in_word
setopt always_to_end

# Ensure that zsh-autosuggestions uses async mode
ZSH_AUTOSUGGEST_USE_ASYNC=true

# Limit the size of which we enable autosuggestions
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

# Use menu as completion list
zstyle ':completion:*:*:*:*:*' menu select

# Completion works when you TAB in the middle of the word
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'

# ((( Those settings were extracted from sheerun dotfiles

# Make autocompletion faster by caching and prefix-only matching
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on

# fuzzy matching of completions for when you mistype them
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# get better autocompletion accuracy by typing longer words
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Ignore completion functions for commands you don't have
zstyle ':completion:*:functions' ignored-patterns '_*'

# )))

# Use LS_COLORS for color completion (requires dir-navigation.zsh)
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Completing process IDs with menu selection
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# Show all processes when completing kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
