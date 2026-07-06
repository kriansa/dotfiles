# Completions for git-wt command
# Fish auto-uses these for "git wt" via __fish_git_complete_custom_command

# Disable file completions by default
complete -c git-wt -f

# Helper: list branches with worktrees (for rm completion)
function __fish_git_wt_branches
  git worktree list --porcelain 2>/dev/null \
  | string match -r '^branch refs/heads/(.+)' \
  | string replace 'branch refs/heads/' ''
end

# Helper: list all git branches (for add completion)
function __fish_git_wt_all_branches
  git for-each-ref --format='%(refname:short)' refs/heads/ 2>/dev/null
end

# Helper: check if subcommand is already specified
function __fish_git_wt_needs_subcommand
  not __fish_seen_subcommand_from add new create rm remove delete del list ls gc
end

# Helper: check if using a specific subcommand
function __fish_git_wt_using_subcommand
  __fish_seen_subcommand_from $argv
end

# Subcommands
complete -c git-wt -n __fish_git_wt_needs_subcommand -a add -d "Create a new worktree"
complete -c git-wt -n __fish_git_wt_needs_subcommand -a rm -d "Remove a worktree"
complete -c git-wt -n __fish_git_wt_needs_subcommand -a list -d "List worktrees"
complete -c git-wt -n __fish_git_wt_needs_subcommand -a gc -d "Prune merged/closed branches & worktrees; reset main"

# add subcommand
complete -c git-wt -n '__fish_git_wt_using_subcommand add new create' -s b -d "Create new branch"
complete -c git-wt -n '__fish_git_wt_using_subcommand add new create' -s h -l help -d "Show help"
complete -c git-wt -n '__fish_git_wt_using_subcommand add new create' -a "(__fish_git_wt_all_branches)"

# rm subcommand
complete -c git-wt -n '__fish_git_wt_using_subcommand rm remove delete del' -s f -l force -d "Force removal"
complete -c git-wt -n '__fish_git_wt_using_subcommand rm remove delete del' -s F -l force-all -d "Force removal + delete branch without prompting"
complete -c git-wt -n '__fish_git_wt_using_subcommand rm remove delete del' -s h -l help -d "Show help"
complete -c git-wt -n '__fish_git_wt_using_subcommand rm remove delete del' -a "(__fish_git_wt_branches)"

# list subcommand
complete -c git-wt -n '__fish_git_wt_using_subcommand list ls' -s h -l help -d "Show help"

# gc subcommand
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l apply -d "Perform deletions (default: dry run)"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l discard-local-changes -d "Also remove dirty worktrees (discards changes)"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l include-closed -d "Also delete closed branches with local-only commits"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l reset-main -d "Reset main worktree to default and fast-forward"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l json -d "Emit the plan as JSON"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l no-fetch -d "Skip git fetch --prune"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l offline -d "Skip gh; classify by [gone] upstream only"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -l no-color -d "Disable colored output"
complete -c git-wt -n '__fish_git_wt_using_subcommand gc' -s h -l help -d "Show help"
