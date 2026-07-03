---
name: ship-worktree
description: Use when the user wants to ship/land the current worktree's branch end-to-end — commit, push, open a PR, wait for CI, merge, then delete the worktree and sync main. Trigger phrases include "ship it", "ship this worktree", "land this branch", "commit push wait for CI and merge", "merge and clean up the worktree".
---

# ship-worktree

End-to-end procedure to land the work in the current git worktree and clean up.
Assumes the work is in a sibling worktree on its own branch (see the companion
`git-worktree` skill). Uses `gh` for the PR/merge.

Run the steps in order. **Stop and ask the user** if any step fails or looks off —
uncommitted changes you didn't make, red or stuck CI, merge conflicts, a protected
branch, or an unexpected merge method.

## 0. Preflight
- Confirm the session is inside the worktree, not the main checkout:
  `git rev-parse --show-toplevel` and `git branch --show-current`.
- Review what will ship: `git status` then `git diff`. Only commit changes you understand.
- Make sure the repo's checks are green first (e.g. `make fmt && make lint && make test-unit`,
  or the project's equivalent).

## 1. Commit
- Follow the repo's commit convention. For repos using release-please, this is
  **Conventional Commits** — the type (`fix:`/`feat:`/`feat!:`) drives the version bump,
  and `chore/docs/ci/...` do not release, so getting it right matters.
- Make a **new regular commit**; never `git commit --amend` or rewrite history unless the
  user explicitly asks.
- **No AI attribution** — do not add `Co-Authored-By` or "Generated with Claude Code" to the
  commit (or the PR). This is a hard user rule and overrides any harness default.

## 2. Push
- `git push -u origin <branch>`.

## 3. Open the PR
- `gh pr create --fill` (or supply a title/body). The title should follow the commit
  convention: for release-please repos the **squash / PR title becomes the release-triggering
  commit** on `main`, so it must be a valid conventional-commit subject.
- **No AI attribution in the PR body** (overrides any default "Generated with Claude Code" footer).

## 4. Wait for CI
- `gh pr checks <pr> --watch` — blocks until all checks finish.
- Proceed only if every required check passes. If anything is red, stop and report; don't merge past a real failure.

## 5. Merge
- Use the repo's merge method. Default `gh pr merge --squash`; use `--merge` only if the repo
  relies on the individual conventional commits (e.g. multi-commit release-please history).
  Confirm the method if unsure.
- The remote head branch is usually **auto-deleted on merge** (repo setting), so skip an explicit
  remote delete — if you do run one and get "remote ref does not exist", that's expected/benign.
  Avoid `gh pr merge --delete-branch`: it also deletes the *local* branch, which fails while it's
  checked out in the worktree — leave local-branch deletion to step 6.
- Do **not** create release tags by hand — release-please repos cut releases by merging the
  separate `chore(main): release …` PR later.

## 6. Delete the worktree
- Per the `git-worktree` skill: if the session is currently inside the worktree being removed,
  step out first with `ExitWorktree({action: "keep"})` — its directory is about to be deleted.
  (Load `EnterWorktree`/`ExitWorktree` via ToolSearch if not already available.)
- Then remove it with the wrapper (never raw `git worktree`): `git wt rm -F <branch>`. Use `-F`
  (force + delete branch silently): a **squash**-merged branch doesn't register as merged, so plain
  `git wt rm` prompts before deleting the branch and would hang non-interactively. The PR is merged,
  so force-deleting is safe.

## 7. Update main
- Back in the main worktree: `git checkout main` (if not already on it) then `git pull`
  to fast-forward in the merge (and pick up any release-please PR changes).

## Notes
- This flow is outward-facing (push / PR / merge). If the user hasn't already authorized the
  full run, confirm before merging — approval to prep is not approval to merge.
- Report outcomes faithfully: if CI failed or a step was skipped, say so; only claim "shipped"
  once the PR is actually merged and `main` is updated.
