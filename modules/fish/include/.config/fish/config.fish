# Add ~/.bin to path
fish_add_path --path --global $HOME/.bin

if status is-interactive
  # Load theme
  source (/usr/bin/starship init fish --print-full-init | psub)

  # Set good defaults for FZF
  # adds ctrl-[hjkl], ctrl-[dufb] for vim-like navigation
  # cycle allows jumping between the first and last results, making scrolling faster
  # layout=reverse lists results top to bottom, mimicking the familiar layouts of git log, history, and env
  # preview-window=wrap wraps long lines in the preview window, making reading easier
  # marker=+ makes the multi-select marker more distinguishable from the pointer (since both default to >)
  set --global --export FZF_DEFAULT_OPTS "--bind=ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-b:page-up,ctrl-f:page-down --cycle --layout=reverse --preview-window=wrap --marker='+'"

  #  Configure fzf.fish
  set --global fzf_fd_opts --hidden --exclude=.git

  # Add navigational helpers
  function ...
  ../..
  end

  function ....
    ../../..
  end
end
