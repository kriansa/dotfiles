# This wraps git so that we have git wt add/rm commands change the current directory
function git --wraps=git
  switch "$argv[1] $argv[2]"
  case "wt rm"
    _git_wt_rm $argv[3..]
  case "wt add"
    _git_wt_add $argv[3..]
  case '*'
    command git $argv
  end
end

function _git_wt_add
  # Run the actual git wt add command
  command git wt add $argv
  or return $status

  # Parse args to find the branch name (skip -b if present)
  set -l branch
  for arg in $argv
    if test "$arg" != "-b"
      set branch $arg
      break
    end
  end

  # Get worktree path from git (query actual data, not heuristic)
  set -l wt_path (git worktree list --porcelain | awk -v branch="refs/heads/$branch" '
  /^worktree / { path = substr($0, 10) }
  /^branch / && $0 == "branch " branch { print path }
  ')

  # cd to new worktree
  if test -n "$wt_path"
    cd $wt_path
  end
end

function _git_wt_rm
  set -l main_wt (git worktree list --porcelain | head -1 | sed 's/^worktree //')
  set -l current_wt (git rev-parse --show-toplevel)

  # Extract branch (first non-flag argument) - flags pass through unchanged
  set -l branch
  for arg in $argv
    if not string match -q -- '-*' $arg
      set branch $arg
      break
    end
  end

  # Track target worktree (for cd-back on failure)
  set -l target_wt
  set -l did_cd false

  if test -z "$branch"
    # No branch specified - current worktree is the target
    if test "$current_wt" = "$main_wt"
      echo "error: not in a worktree (currently in main)" >&2
      return 1
    end

    # Get branch for current worktree from porcelain output
    set branch (git worktree list --porcelain | awk -v wt="$current_wt" '
    /^worktree / { current = substr($0, 10) }
    /^branch / && current == wt { print substr($0, 19) }
    ')

    # Prompt user
    read -P "Delete worktree for branch '$branch'? [y/N] " -l response
    if not string match -qi 'y' $response
      return 0
    end

    # We're in the target, cd to main
    set target_wt $current_wt
    cd $main_wt
    set did_cd true
  else
    # Branch specified - get its worktree path from git
    set target_wt (git worktree list --porcelain | awk -v branch="refs/heads/$branch" '
    /^worktree / { path = substr($0, 10) }
    /^branch / && $0 == "branch " branch { print path }
    ')

    if test "$current_wt" = "$target_wt"
      cd $main_wt
      set did_cd true
    end
  end

  # Run the actual rm command - pass all original args, add branch if it was inferred
  set -l rm_status
  if test (count $argv) -eq 0
    command git wt rm $branch
    set rm_status $status
  else
    command git wt rm $argv
    set rm_status $status
  end

  # If rm failed and worktree still exists, cd back
  if test $rm_status -ne 0 -a "$did_cd" = true -a -d "$target_wt"
    cd $target_wt
  end

  return $rm_status
end
