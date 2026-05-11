---
name: git-worktree
description: Use whenever the user wants to create, remove, or list git worktrees, or asks to work on a branch in parallel without disturbing the current checkout. The user has a custom wrapper called `git-wt` — always use it instead of `git worktree` directly.
---

# git-worktree

The user has a custom git worktree wrapper called `git-wt` (source: `~/.dotfiles/modules/git/bin/git-wt`). It enforces opinionated conventions and adds safety/UX around vanilla `git worktree`. **Always use `git wt` instead of `git worktree` for worktree operations.** Do not fall back to raw `git worktree` — if a request seems to require a flag `git wt` doesn't support, surface that to the user.

## Commands

### Create a worktree — `git wt add`

```bash
git wt add <branch>          # check out an existing branch into a new worktree
git wt add -b <new-branch>   # create the branch and check it out in a new worktree
```

Aliases: `add`, `new`, `create`.

The new worktree is created as a **sibling** to the main worktree, with slashes in the branch name replaced by dashes:

| Main worktree | Branch | New worktree path |
| --- | --- | --- |
| `~/code/myrepo` | `feature/foo` | `~/code/myrepo+feature-foo` |
| `~/code/myrepo` | `bugfix` | `~/code/myrepo+bugfix` |

### Remove a worktree — `git wt rm`

```bash
git wt rm <branch>     # refuses if path is in use; prompts before deleting branch
git wt rm -f <branch>  # force: skip in-use check, rm -rf leftovers
git wt rm -F <branch>  # force + delete branch silently
git wt rm              # remove the current worktree (refuses if you're in main)
```

Aliases: `rm`, `remove`, `delete`, `del`.

### List worktrees — `git wt list`

```bash
git wt list   # or: git wt ls
```

Delegates to `git worktree list`.

## Quirks Claude must respect

1. **Auto-cd is fish-only.** The user's interactive fish shell wraps `git wt add` and `git wt rm` to `cd` automatically. Claude's Bash tool runs bash, so when *you* invoke `git wt add`, you stay in the original directory. If subsequent steps must run in the new worktree, `cd` explicitly:

   ```bash
   git wt add feature/foo
   cd "$(git worktree list --porcelain | awk '/^worktree / { p=$2 } /^branch refs\/heads\/feature\/foo$/ { print p; exit }')"
   ```

   Or, since the path is deterministic, compute it: `<dirname-of-main>/<basename-of-main>+<branch-with-/-as->`.

2. **`git wt rm` without a branch argument targets the current worktree.** Convenient interactively, dangerous in scripts. **Always pass the branch explicitly** when invoking from tools.

3. **First worktree creation prompts on stdin** to install a post-checkout hook. If you're running non-interactively and want to skip that prompt, install the hook (or an empty placeholder) at `.git/hooks/post-checkout` before calling `git wt add`. See "Post-checkout hook" below.

4. **In-use check uses `lsof +D <path>`.** If a tmux pane, editor, or shell has the worktree open, `git wt rm` refuses. Use `-f` only after confirming nothing important is open. If the user is currently *in* the worktree being removed, the fish wrapper handles the cd-out — but Claude should still pass `-f` cautiously.

5. **The `core.hooksPath` override is automatic.** `git wt add` temporarily resets `core.hooksPath` to the default so the post-checkout hook fires even in repos with custom hook paths. Don't fight this.

## Post-checkout hook

`git-wt` runs `.git/hooks/post-checkout` after every new worktree creation. The first time the user adds a worktree in a repo, `git-wt` offers to install a starter hook from `~/.dotfiles/modules/git/data/post-checkout.sample`.

The template defines a `setup()` function and three helpers:

| Helper | Behavior |
| --- | --- |
| `copy <file_or_dir>` | `cp -a` from the source worktree to the new one |
| `link <file_or_dir>` | `ln -sf` from the source worktree to the new one |
| `run <command>` | Run a command in the new worktree's directory |

Typical setup for a JS/Node repo:

```bash
setup() {
    link .env
    copy .claude/settings.local.json
    run pnpm install
}
```

Hooks live under `.git/` so they are per-clone and not committed. When the user asks to "set up worktree auto-init" for a repo:

1. Copy the template into `.git/hooks/post-checkout`.
2. `chmod +x` it.
3. Edit `setup()` to suit the repo (ask the user what should be linked, copied, or run).

## When to invoke this skill

Trigger phrases include:

- "create a worktree for `<branch>`"
- "make a new worktree off `main`"
- "spin up a parallel checkout"
- "delete the worktree for `<branch>`" / "remove this worktree"
- "list my worktrees"
- "set up worktree init for this repo" (→ post-checkout hook)
- Any mention of working on multiple branches simultaneously without stashing
