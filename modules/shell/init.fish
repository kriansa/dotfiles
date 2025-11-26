status is-interactive || return

# Set good defaults for FZF
# adds ctrl-[hjkl], ctrl-[dufb] for vim-like navigation
# cycle allows jumping between the first and last results, making scrolling faster
# layout=reverse lists results top to bottom, mimicking the familiar layouts of git log, history, and env
# preview-window=wrap wraps long lines in the preview window, making reading easier
# marker=+ makes the multi-select marker more distinguishable from the pointer (since both default to >)
set --global --export FZF_DEFAULT_OPTS "\
  --color=bg+:#ccd0da,bg:#eff1f5,spinner:#dc8a78,hl:#d20f39 \
  --color=fg:#4c4f69,header:#d20f39,info:#8839ef,pointer:#dc8a78 \
  --color=marker:#7287fd,fg+:#4c4f69,prompt:#8839ef,hl+:#d20f39 \
  --color=selected-bg:#bcc0cc \
  --bind=ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-b:page-up,ctrl-f:page-down \
  --cycle --multi --layout=reverse --preview-window=wrap --marker='+'"

#  Configure fzf.fish
set --global fzf_fd_opts --hidden --exclude=.git

# Configure theme (Hydro)
set --global hydro_multiline true
set --global hydro_cmd_duration_threshold 500
set --global hydro_symbol_git_dirty "*"
set --global hydro_color_pwd blue
set --global hydro_color_git brblack
set --global hydro_color_prompt magenta
set --global hydro_color_duration yellow
set --global hydro_color_error red
set --global fish_prompt_pwd_dir_length 10

# Custom color
set --global hydro_color_jobs cyan

# Add navigational helpers
# (these functions couldn't be in their dedicated files because they would have weird names)
function ...
  ../..
end

function ....
  ../../..
end

alias cat=bat

# This sets the color scheme of bat for a more pleasant in light terminals
set --global --export BAT_THEME OneHalfLight

# Disable cowsay for ansible
set --global --export ANSIBLE_NOCOWS 1
