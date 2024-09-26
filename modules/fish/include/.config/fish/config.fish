# This file (config.fish) is loaded after all conf.d ones

# Add ~/.bin to path as the first in the list so it overrides existing system executables
fish_add_path --path --move --global $HOME/.bin

if status is-interactive
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

  # Configure theme (Pure)
  set --global pure_show_jobs true
  set --global pure_threshold_command_duration 1
  set --global pure_show_subsecond_command_duration true
  set --global pure_enable_container_detection false
  set --global async_prompt_functions _pure_prompt_git
end
