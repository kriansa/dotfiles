function c --description="Quickly jumps into a project's directory"
  cd "$PROJECTS/$argv[1]"

  if test $status -eq 0; and test -n "$argv[1]"
    set -l window_name (string trim --chars=/ $argv[1])

    # Check if we're in a git worktree
    set -l worktree_list (git worktree list 2>/dev/null)
    if test $status -eq 0; and test -n "$worktree_list"
      # First line is the main worktree
      set -l main_worktree (echo $worktree_list[1] | string split ' ')[1]
      set -l current_dir (pwd -P)

      # If current dir is not the main worktree, use main worktree's basename with asterisk
      if test "$current_dir" != "$main_worktree"
        set window_name (basename $main_worktree)"*"
      end
    end

    tmux rename-window $window_name
  end
end
