# Ignore this function if not being run interactively at tmux
if status is-interactive && test -z "$TMUX"
  return
end

function set_tmux_cwd --on-event fish_prompt
  tmux set -q "@cwd_$TMUX_PANE" $PWD
end

# During Fish initialization, PATH value can be changed and should be reflected on tmux's own
# internal representation of PATH, which is used for when we need to use `tmux run`. For this reason
# we set tmux internal PATH after all fish plugins have loaded and modified the path so we have a
# consistent value across the shell and tmux.
function set_tmux_path --on-event path_modified
  tmux setenv PATH "$PATH"
end
