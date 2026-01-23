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
set --global fzf_preview_dir_cmd eza --all --color=always
set --global fzf_diff_highlighter delta --paging=never --width=20

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

# Set theme based on fish 3.0
set --global fish_color_autosuggestion 555 brblack
set --global fish_color_cancel -r
set --global fish_color_command 005fd7
set --global fish_color_comment 990000
set --global fish_color_cwd green
set --global fish_color_cwd_root red
set --global fish_color_end 009900
set --global fish_color_error ff0000
set --global fish_color_escape 00a6b2
set --global fish_color_history_current --bold
set --global fish_color_host normal
set --global fish_color_host_remote yellow
set --global fish_color_normal normal
set --global fish_color_operator 00a6b2
set --global fish_color_param 00afff
set --global fish_color_quote 999900
set --global fish_color_redirection 00afff
set --global fish_color_search_match white --background=brblack
set --global fish_color_selection white --bold --background=brblack
set --global fish_color_status red
set --global fish_color_user brgreen
set --global fish_color_valid_path --underline
set --global fish_pager_color_completion
set --global fish_pager_color_description B3A06D yellow
set --global fish_pager_color_prefix normal --bold --underline
set --global fish_pager_color_progress brwhite --background=cyan
set --global fish_pager_color_selected_background -r

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
