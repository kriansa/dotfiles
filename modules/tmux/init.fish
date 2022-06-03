# Ignore this function if not being run interactively at tmux
if status is-interactive && test -z "$TMUX"
  return
end

function set_tmux_cwd --on-event fish_prompt
  tmux set -q "@cwd_$TMUX_PANE" $PWD
end
