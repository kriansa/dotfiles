# Git plugin
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# 1. Configs
# Query/use custom command for `git`.
zstyle -s ":vcs_info:git:*:-all-" "command" _omz_git_git_cmd
: ${_omz_git_git_cmd:=git}

# 2. Useful aliases
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit -m "--wip--"'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
