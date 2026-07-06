---
name: git-worktree
description: Use whenever the user wants to create, remove, or list git worktrees, work on a branch in parallel without disturbing the current checkout, or clean up / prune / garbage-collect / tidy merged or closed branches and their worktrees (including resetting the main worktree back to the default branch and syncing it). The user has a custom wrapper called `git-wt` - always use it (`git wt add` / `rm` / `list` / `gc`) instead of `git worktree` directly.
---

# git-worktree

The user has a custom git worktree wrapper called `git-wt` (source: `~/.dotfiles/modules/git/bin/git-wt`, a single stdlib Python/uv tool). It enforces opinionated conventions and adds safety/UX around vanilla `git worktree`. **Always use `git wt` instead of `git worktree` for worktree operations.** Do not fall back to raw `git worktree` - if a request seems to require a flag `git wt` doesn't support, surface that to the user.

Subcommands: `add` / `rm` / `list` (below), plus **`gc`** - bulk-prune merged/closed branches & worktrees and reset the main worktree (see "Clean up" near the end).

## Commands

### Create a worktree - `git wt add`

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

**After creating, switch the *session* into the new worktree** so follow-on work runs there - don't use a Bash `cd` (it moves only Claude's shell, not the session). Use the `EnterWorktree` harness tool with the worktree's **`path`** (never `name` - `name` would create a *separate* `.claude/worktrees/` worktree instead of entering the one `git wt` just made):

1. Resolve the new worktree's absolute path (use the branch name; for `-b`, that's the new branch):

   ```bash
   git worktree list --porcelain | awk -v b="refs/heads/<branch>" '/^worktree / { p = substr($0, 10) } $0 == "branch " b { print p; exit }'
   ```

2. If the session is **already inside another worktree**, `ExitWorktree({action: "keep"})` first to return to the main worktree - switching straight into a sibling worktree via `path` is rejected while you're in one.
3. `EnterWorktree({path: "<that-path>"})`. This moves the session *and* the Bash tool's cwd into the worktree (interactive fish does the same cd on its own). If it's still rejected, fall back to telling the user to run `/cd <that-path>`.

### Remove a worktree - `git wt rm`

```bash
git wt rm <branch>     # refuses if path is in use; prompts before deleting branch
git wt rm -f <branch>  # force: skip in-use check, rm -rf leftovers
git wt rm -F <branch>  # force + delete branch silently
git wt rm              # remove the current worktree (refuses if you're in main)
```

Aliases: `rm`, `remove`, `delete`, `del`.

**If the session is currently inside the worktree being removed, step out first** - its directory is about to be deleted. Don't stop to ask; call `ExitWorktree({action: "keep"})` to return the session to the main worktree (the worktree stays on disk for `git wt rm` to delete). Use `keep`, never `remove` - `git wt rm` owns the deletion and its safety checks.

1. Resolve the target worktree's path with the awk query above. For the no-arg form, the target is the current worktree (`git rev-parse --show-toplevel`, branch from `git branch --show-current`) - always pass the branch explicitly to `git wt rm` (see Quirk 2).
2. If `git rev-parse --show-toplevel` equals the target path, `ExitWorktree({action: "keep"})`.
3. Run `git wt rm <branch>`.

If `ExitWorktree` reports no active worktree session (you entered the worktree some other way - e.g. launched Claude inside it), it's a no-op and you'll still be in the path; `git wt rm` then refuses via its in-use check, so ask the user to move out (`/cd <main-worktree-path>`) and retry.

### List worktrees - `git wt list`

```bash
git wt list   # or: git wt ls
```

Delegates to `git worktree list`.

## Clean up merged/closed worktrees - `git wt gc`

Bulk garbage-collector for a checkout that has piled up stale branches and worktrees. Run
it from inside the repo to clean - it operates on that repo's **main worktree**. `git wt gc`
fetches `--prune`, resolves the right GitHub account from the repo's SSH identity, then
classifies every local branch by its **PR state** (squash-proof, unlike `git branch --merged`):

| Bucket | What it is | Default action |
| --- | --- | --- |
| clean merged/closed | MERGED, or CLOSED with commits still reachable on a remote, worktree clean | deleted on `--apply` |
| dirty | merged/closed but the worktree has uncommitted/untracked changes | opt-in: `--discard-local-changes` |
| closed local-only | CLOSED and the commits exist only locally | opt-in: `--include-closed` |
| kept | OPEN PR, or no PR at all (local-only branches, review checkouts) | never touched |
| reset-main | the main worktree's own current branch | `--reset-main` |

`--reset-main` moves the main worktree's current branch into its own sibling worktree (or
deletes it if it's a clean merged/closed candidate), switches the main worktree back to the
default branch, and fast-forwards it against origin. **Only `--discard-local-changes` ever discards
uncommitted work**; untracked files in the main worktree are never moved or deleted.

### How to drive it

1. **Preview** - `git wt gc --json` from the target repo. Summarize the buckets for the user.
2. **Delete the safe ones right away** - `git wt gc --apply` removes the clean, provably
   merged/closed branches/worktrees and reports the opt-in buckets that remain.
3. **Per-category approval** - for each non-empty opt-in bucket, ask the user
   (AskUserQuestion), listing the specific branches:
   - dirty → run with `--discard-local-changes`? Their uncommitted/untracked changes are discarded.
   - closed local-only → run with `--include-closed`? Abandoned local commits are lost.
   - reset-main → relocate the current branch, reset main to the default branch, and sync?
4. **Apply the approved categories** - re-run with the approved flags plus `--no-fetch`
   (step 2 already fetched), e.g. `git wt gc --apply --no-fetch --discard-local-changes --reset-main`.
5. **Report faithfully** - deleted / relocated / kept / skipped, plus the sync result. Each
   `--json` item carries a `result` (and `error` on failure).

### Notes / gotchas

- **gh account:** `gc` switches the active `gh` account to the repo's SSH identity
  (`ssh -T` → "Hi &lt;login&gt;!") for its queries and restores the previous account on exit. If it
  can't (no matching gh account, gh missing, or an https remote), it warns and degrades to
  **`[gone]`-only** classification - which can't see CLOSED PRs and misses merged-but-not-pruned
  branches, so cleanup is partial. Prefer fixing the account over trusting a degraded run.
- **Sync is fast-forward only.** If the default branch has diverged, it reports that and skips.
- **Standalone use:** run `git wt gc` (dry-run) or `git wt gc --apply ...` directly.

## Quirks Claude must respect

1. **Move the session with `EnterWorktree`/`ExitWorktree`, not Bash `cd`.** The user's interactive fish shell auto-cds on `git wt add`/`git wt rm`; Claude's Bash tool runs bash and doesn't - and a Bash `cd` moves only Claude's shell, not the session (the user's view, `CLAUDE.md`, `/resume`). Use the harness worktree tools: `EnterWorktree({path})` after `git wt add` (see "Create a worktree"), and `ExitWorktree({action: "keep"})` before removing the worktree you're in (see "Remove a worktree"). They're deferred tools - if not already available, load them first with `ToolSearch("select:EnterWorktree,ExitWorktree")`.

2. **`git wt rm` without a branch argument targets the current worktree.** Convenient interactively, dangerous in scripts. **Always pass the branch explicitly** when invoking from tools.

3. **First worktree creation prompts on stdin** to install a post-checkout hook. If you're running non-interactively and want to skip that prompt, install the hook (or an empty placeholder) at `.git/hooks/post-checkout` before calling `git wt add`. See "Post-checkout hook" below.

4. **In-use check uses `lsof +D <path>`.** If a tmux pane, editor, or shell has the worktree open, `git wt rm` refuses. Use `-f` only after confirming nothing important is open. If the session is currently *in* the worktree being removed, step out with `ExitWorktree({action: "keep"})` before removing (see "Remove a worktree") - don't reach for `-f` just to get past your own shell.

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
- "clean up / prune merged or closed branches and worktrees" / "garbage-collect worktrees" / "tidy up this checkout" (→ `git wt gc`)
- "get the main worktree back on main and sync it" / "delete branches whose PRs have landed" (→ `git wt gc`)
