"""Unit tests for the git-wt tool (stdlib unittest, no third-party deps).

The tool is an extension-less PEP 723 uv script, so it is loaded as a module by
path. Its top level only defines things (guarded by __main__), so importing is safe.
Run: python3 -m unittest discover -s modules/git/tests -t modules/git/tests -v
"""

# The tool is loaded dynamically by path, so its symbols (e.g. git_wt.Git) are not
# statically resolvable by mypy; types are exercised at runtime instead.
# mypy: ignore-errors

import contextlib
import importlib.machinery
import importlib.util
import io
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

# Don't write a .pyc into modules/git/bin/__pycache__ - dotup links everything under a
# module's bin/, and that cache dir is not a tool to put on PATH.
sys.dont_write_bytecode = True

_TOOL = Path(__file__).resolve().parents[1] / "bin" / "git-wt"
_loader = importlib.machinery.SourceFileLoader("git_wt", str(_TOOL))
_spec = importlib.util.spec_from_loader("git_wt", _loader)
assert _spec is not None
git_wt = importlib.util.module_from_spec(_spec)
_loader.exec_module(git_wt)

git_wt.use_color(False)  # deterministic, un-colored output in assertions


# --------------------------------------------------------------------------------------
# Builders & fakes
# --------------------------------------------------------------------------------------


def make_porcelain(entries: list[tuple[str, str | None]], head: str = "abc123") -> str:
    """entries: list of (path, branch-or-None). None → a detached stanza."""
    blocks = []
    for path, branch in entries:
        lines = [f"worktree {path}", f"HEAD {head}"]
        lines.append("detached" if branch is None else f"branch refs/heads/{branch}")
        blocks.append("\n".join(lines))
    return "\n\n".join(blocks) + "\n"


def make_refs(entries: list[tuple[str, str, str]]) -> str:
    """entries: list of (name, sha, upstream-track)."""
    return "\n".join(f"{name}\t{sha}\t{track}" for name, sha, track in entries) + "\n"


def worktree(path: str, branch: str | None) -> "git_wt.Worktree":
    return git_wt.Worktree(path, "abc123", branch)


def bstate(
    name,
    *,
    pr_state=None,
    pr_num=1,
    gone=False,
    dirty=0,
    recoverable=True,
    wt=None,
) -> "git_wt.BranchState":
    branch = git_wt.Branch(name, f"{name}-sha", gone, wt)
    pr = git_wt.PrInfo(pr_num, pr_state, None) if pr_state else None
    return git_wt.BranchState(branch, pr, dirty, recoverable)


def _cp(stdout="", rc=0, stderr=""):
    return subprocess.CompletedProcess([], rc, stdout, stderr)


class FakeGit(git_wt.Git):
    """Records every git command and returns canned output; never runs git."""

    def __init__(
        self,
        *,
        porcelain="",
        refs="",
        statuses=None,
        toplevel="/repo",
        common_dir="/repo/.git",
        configs=None,
        local_configs=None,
        remote="git@github.com:acme/repo.git",
        default_head="main",
        verify=(),
        ancestors=(),
        contains=None,
        counts=None,
        fetch_rc=0,
        action_rc=None,
    ):
        super().__init__("/repo")
        self.commands: list[list[str]] = []
        self.porcelain = porcelain
        self.refs = refs
        self.statuses = statuses or {}
        self.toplevel = toplevel
        self.common_dir = common_dir
        self.configs = configs or {}
        self.local_configs = local_configs or {}
        self.remote = remote
        self.default_head = default_head
        self.verify = set(verify)
        self.ancestors = set(ancestors)
        self.contains = contains or {}
        self.counts = counts or {}
        self.fetch_rc = fetch_rc
        self.action_rc = action_rc or {}

    def _ok(self, args, *, cwd=None):
        return self._run(args, cwd=cwd).returncode == 0

    def _run(self, args, *, cwd=None, stdin_devnull=False):
        self.commands.append(list(args))
        m = list(args)
        if m[:3] == ["worktree", "list", "--porcelain"]:
            return _cp(self.porcelain)
        if m[:2] == ["worktree", "list"]:
            return _cp(self.porcelain)
        if m[0] == "for-each-ref":
            return _cp(self.refs)
        if m[:2] == ["status", "--porcelain"]:
            return _cp(self.statuses.get(cwd, ""))
        if m[:2] == ["rev-parse", "--show-toplevel"]:
            return _cp(self.toplevel + "\n")
        if m[0] == "rev-parse" and "--git-common-dir" in m:
            return _cp(self.common_dir + "\n")
        if m[:2] == ["rev-parse", "--verify"]:
            return _cp("", 0 if m[2] in self.verify else 1)
        if m[0] == "rev-parse":
            return _cp(m[1] + "\n")
        if m[0] == "config":
            return self._config(m)
        if m[:2] == ["remote", "get-url"]:
            return _cp(self.remote + "\n")
        if m[0] == "symbolic-ref":
            if self.default_head:
                return _cp(f"refs/remotes/origin/{self.default_head}\n")
            return _cp("", 1)
        if m[:2] == ["merge-base", "--is-ancestor"]:
            return _cp("", 0 if (m[2], m[3]) in self.ancestors else 1)
        if m[:3] == ["branch", "-r", "--contains"]:
            return _cp("\n".join(self.contains.get(m[3], [])))
        if m[:2] == ["rev-list", "--count"]:
            return _cp(str(self.counts.get(m[2], 0)) + "\n")
        if m[:2] == ["fetch", "--prune"]:
            return _cp("", self.fetch_rc)
        if m[:2] == ["branch", "-D"]:
            return _cp("", self.action_rc.get(("branch-D", m[2]), 0))
        if m[0] == "switch":
            self._switch_main(m[1])
            return _cp("", self.action_rc.get(("switch", m[1]), 0))
        if m[:2] == ["merge", "--ff-only"]:
            return _cp("", self.action_rc.get(("merge", m[2]), 0))
        if m[:2] == ["worktree", "add"]:
            branch = m[3] if len(m) > 3 and m[2] == "-b" else m[-1]
            return _cp("", self.action_rc.get(("wt-add", branch), 0))
        return _cp("")

    def _config(self, m):
        if "--unset" in m:
            return _cp("")
        if "--get" in m:
            key = m[-1]
            store = self.local_configs if "--local" in m else self.configs
            value = store.get(key)
            return _cp(f"{value}\n" if value is not None else "", 0 if value is not None else 1)
        return _cp("")  # a set

    def _switch_main(self, branch):
        # Model `git switch` re-pointing the main worktree (first porcelain stanza).
        trees = git_wt.Worktree.parse_list(self.porcelain)
        if trees:
            trees[0].branch = branch
            self.porcelain = make_porcelain([(t.path, t.branch) for t in trees])

    def issued(self, *prefix):
        return [c for c in self.commands if c[: len(prefix)] == list(prefix)]


class FakeGh(git_wt.Gh):
    def __init__(self, *, states=None, active="me", accounts=("me",), switch_ok=True, avail=True):
        self._states = states
        self._active = active
        self._accounts = list(accounts)
        self._switch_ok = switch_ok
        self._avail = avail
        self.switches: list[str] = []

    def available(self):
        return self._avail

    def pr_states(self, owner_repo, names):
        return self._states

    def active_login(self):
        return self._active

    def accounts(self):
        return self._accounts

    def switch(self, host, user):
        self.switches.append(user)
        self._active = user
        return self._switch_ok


# --------------------------------------------------------------------------------------
# Pure parsing
# --------------------------------------------------------------------------------------


class TestRemote(unittest.TestCase):
    def test_scp_like(self):
        r = git_wt.Remote.parse("git@github.com:acme/repo.git")
        self.assertEqual(
            (r.ssh_target, r.host, r.owner_repo), ("git@github.com", "github.com", "acme/repo")
        )

    def test_host_alias_preserved(self):
        r = git_wt.Remote.parse("git@github.com-work:acme/repo.git")
        self.assertEqual(r.ssh_target, "git@github.com-work")
        self.assertEqual(r.host, "github.com-work")

    def test_ssh_url_with_port(self):
        r = git_wt.Remote.parse("ssh://git@example.com:2222/acme/repo.git")
        self.assertEqual(
            (r.ssh_target, r.ssh_port, r.owner_repo), ("git@example.com", "2222", "acme/repo")
        )

    def test_https_has_no_ssh_target(self):
        r = git_wt.Remote.parse("https://github.com/acme/repo.git")
        self.assertIsNone(r.ssh_target)
        self.assertEqual(r.owner_repo, "acme/repo")


class TestSshGreeting(unittest.TestCase):
    def test_extracts_login(self):
        text = "Hi daniel-salsa! You've successfully authenticated, but GitHub does not..."
        self.assertEqual(git_wt.parse_ssh_greeting(text), "daniel-salsa")

    def test_no_greeting(self):
        self.assertIsNone(git_wt.parse_ssh_greeting("Permission denied (publickey)."))


class TestWorktreeParse(unittest.TestCase):
    def test_parses_branches_and_detached(self):
        porcelain = make_porcelain(
            [("/repo", "main"), ("/repo+foo", "feat/foo"), ("/repo+d", None)]
        )
        trees = git_wt.Worktree.parse_list(porcelain)
        self.assertEqual([t.branch for t in trees], ["main", "feat/foo", None])
        self.assertEqual(trees[0].path, "/repo")


class TestBranchParse(unittest.TestCase):
    def test_gone_and_worktree_link(self):
        wts = [worktree("/repo+foo", "foo")]
        refs = make_refs([("foo", "s1", "[ahead 1]"), ("bar", "s2", "[gone]")])
        branches = git_wt.Branch.parse(refs, wts)
        foo, bar = branches
        self.assertFalse(foo.gone)
        self.assertEqual(foo.worktree.path, "/repo+foo")
        self.assertTrue(bar.gone)
        self.assertIsNone(bar.worktree)


# --------------------------------------------------------------------------------------
# Classifier (pure)
# --------------------------------------------------------------------------------------


class TestClassifier(unittest.TestCase):
    def setUp(self):
        self.main = worktree("/repo", "feature")
        self.classifier = git_wt.Classifier()

    def classify(self, states, *, tracked=False):
        return self.classifier.classify(
            states, default="main", main=self.main, self_wt="/repo", main_tracked_dirty=tracked
        )

    def names(self, bucket):
        return [c.branch for c in bucket]

    def test_merged_clean_is_tier_a(self):
        plan = self.classify([bstate("m", pr_state="MERGED")])
        self.assertEqual(self.names(plan.tier_a), ["m"])

    def test_closed_recoverable_is_tier_a(self):
        plan = self.classify([bstate("c", pr_state="CLOSED", recoverable=True)])
        self.assertEqual(self.names(plan.tier_a), ["c"])

    def test_closed_local_only_needs_optin(self):
        plan = self.classify([bstate("c", pr_state="CLOSED", recoverable=False)])
        self.assertEqual(self.names(plan.pending_closed_local), ["c"])
        self.assertEqual(plan.tier_a, [])

    def test_merged_dirty_needs_optin(self):
        plan = self.classify([bstate("m", pr_state="MERGED", dirty=2, wt=worktree("/repo+m", "m"))])
        self.assertEqual(self.names(plan.pending_dirty), ["m"])

    def test_gone_no_pr_is_tier_a_when_recoverable(self):
        plan = self.classify([bstate("g", gone=True, recoverable=True)])
        self.assertEqual(self.names(plan.tier_a), ["g"])

    def test_gone_unrecoverable_needs_optin(self):
        plan = self.classify([bstate("g", gone=True, recoverable=False)])
        self.assertEqual(self.names(plan.pending_closed_local), ["g"])

    def test_open_is_kept(self):
        plan = self.classify([bstate("o", pr_state="OPEN")])
        self.assertEqual(self.names(plan.kept), ["o"])
        self.assertEqual(plan.kept[0].reason, "open PR")

    def test_no_pr_no_gone_is_kept(self):
        plan = self.classify([bstate("x")])
        self.assertEqual(self.names(plan.kept), ["x"])

    def test_default_branch_skipped(self):
        plan = self.classify([bstate("main", pr_state="MERGED")])
        self.assertEqual(plan.tier_a, [])
        self.assertEqual(plan.kept, [])

    def test_self_worktree_kept(self):
        st = bstate("s", pr_state="MERGED", wt=worktree("/repo+s", "s"))
        plan = self.classifier.classify(
            [st], default="main", main=self.main, self_wt="/repo+s", main_tracked_dirty=False
        )
        self.assertEqual(self.names(plan.kept), ["s"])
        self.assertIn("running from this worktree", plan.kept[0].reason)

    def test_reset_relocate_for_open_current(self):
        plan = self.classify([bstate("feature", pr_state="OPEN")])
        self.assertEqual(plan.reset.disposition, "relocate")
        self.assertEqual(plan.reset.target, "/repo+feature")

    def test_reset_delete_for_merged_current(self):
        plan = self.classify([bstate("feature", pr_state="MERGED", recoverable=True)])
        self.assertEqual(plan.reset.disposition, "delete")

    def test_reset_already_default(self):
        main = worktree("/repo", "main")
        plan = self.classifier.classify(
            [], default="main", main=main, self_wt="/repo", main_tracked_dirty=False
        )
        self.assertEqual(plan.reset.disposition, "already-default")

    def test_reset_blocked_when_tracked_dirty(self):
        plan = self.classify([bstate("feature", pr_state="OPEN")], tracked=True)
        self.assertEqual(plan.reset.disposition, "blocked-dirty")

    def test_reset_detached(self):
        main = worktree("/repo", None)
        plan = self.classifier.classify(
            [], default="main", main=main, self_wt="/repo", main_tracked_dirty=False
        )
        self.assertEqual(plan.reset.disposition, "detached")


# --------------------------------------------------------------------------------------
# AccountResolver
# --------------------------------------------------------------------------------------


class TestAccountResolver(unittest.TestCase):
    def _resolver(self, git, gh, *, offline=False, login="daniel"):
        return git_wt.AccountResolver(git, gh, offline=offline, identity=lambda remote, cmd: login)

    def test_offline_is_degraded(self):
        ctx = self._resolver(FakeGit(), FakeGh(), offline=True)._resolve()
        self.assertEqual(ctx.mode, "degraded")
        self.assertEqual(ctx.reason, "offline mode requested")

    def test_gh_unavailable_is_degraded(self):
        ctx = self._resolver(FakeGit(), FakeGh(avail=False))._resolve()
        self.assertEqual(ctx.mode, "degraded")

    def test_https_uses_active_account(self):
        git = FakeGit(remote="https://github.com/acme/repo.git")
        ctx = self._resolver(git, FakeGh(active="whoever"))._resolve()
        self.assertEqual((ctx.mode, ctx.login), ("gh", "whoever"))

    def test_switches_to_ssh_identity(self):
        gh = FakeGh(active="kriansa", accounts=("kriansa", "daniel"))
        ctx = self._resolver(FakeGit(), gh, login="daniel")._resolve()
        self.assertEqual((ctx.mode, ctx.login, ctx.switched_from), ("gh", "daniel", "kriansa"))
        self.assertEqual(gh.switches, ["daniel"])

    def test_identity_not_a_known_account_is_degraded(self):
        gh = FakeGh(active="kriansa", accounts=("kriansa",))
        ctx = self._resolver(FakeGit(), gh, login="daniel")._resolve()
        self.assertEqual(ctx.mode, "degraded")
        self.assertIn("daniel", ctx.reason)

    def test_activated_restores_previous_account(self):
        gh = FakeGh(active="kriansa", accounts=("kriansa", "daniel"))
        resolver = self._resolver(FakeGit(), gh, login="daniel")
        with resolver.activated() as ctx:
            self.assertEqual(ctx.login, "daniel")
        self.assertEqual(gh.switches, ["daniel", "kriansa"])  # switched then restored


# --------------------------------------------------------------------------------------
# Worktrees
# --------------------------------------------------------------------------------------


class TestWorktreesAdd(unittest.TestCase):
    def _wt(self, git):
        return git_wt.Worktrees(
            git, confirm=lambda p: False, path_in_use=lambda p: False, template=Path("/nonexistent")
        )

    def test_add_issues_worktree_add(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main")]))
        with contextlib.redirect_stdout(io.StringIO()):
            self._wt(git).add("feat/x", create=True)
        add = git.issued("worktree", "add")
        self.assertEqual(add, [["worktree", "add", "-b", "feat/x", "/repo+feat-x"]])

    def test_add_refuses_existing_branch_worktree(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main"), ("/repo+x", "x")]))
        with self.assertRaises(git_wt.GitWtError):
            self._wt(git).add("x")

    def test_add_refuses_existing_path(self):
        tmp = Path(self.enterContext(tempfile.TemporaryDirectory()))
        (tmp / "repo").mkdir()
        (tmp / "repo+x").mkdir()  # the sibling path already exists on disk
        git = FakeGit(porcelain=make_porcelain([(str(tmp / "repo"), "main")]))
        with self.assertRaises(git_wt.GitWtError):
            self._wt(git).add("x")

    def test_add_surfaces_git_failure(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main")]), action_rc={("wt-add", "x"): 1})
        with self.assertRaises(git_wt.GitWtError), contextlib.redirect_stdout(io.StringIO()):
            self._wt(git).add("x")

    def test_hooks_path_override_restored(self):
        git = FakeGit(
            porcelain=make_porcelain([("/repo", "main")]),
            common_dir="/repo/.git",
            configs={"core.hooksPath": "/custom/hooks"},
        )
        with contextlib.redirect_stdout(io.StringIO()):
            self._wt(git).add("x")
        # It set the local hooksPath to the default around the add, then unset it (no prior local).
        self.assertIn(["config", "--local", "core.hooksPath", "/repo/.git/hooks"], git.commands)
        self.assertIn(["config", "--local", "--unset", "core.hooksPath"], git.commands)


class TestWorktreesRemove(unittest.TestCase):
    def _wt(self, git, *, confirm=lambda p: False, in_use=lambda p: False):
        return git_wt.Worktrees(git, confirm=confirm, path_in_use=in_use)

    def test_force_all_removes_worktree_and_branch(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main"), ("/repo+x", "x")]))
        with contextlib.redirect_stdout(io.StringIO()):
            self._wt(git).remove("x", force_all=True)
        self.assertTrue(git.issued("worktree", "remove", "--force"))
        self.assertIn(["branch", "-D", "x"], git.commands)

    def test_missing_worktree_raises(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main")]))
        with self.assertRaises(git_wt.GitWtError):
            self._wt(git).remove("ghost")

    def test_in_use_without_force_raises(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main"), ("/repo+x", "x")]))
        with self.assertRaises(git_wt.GitWtError):
            self._wt(git, in_use=lambda p: True).remove("x")

    def test_interactive_decline_keeps_branch(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main"), ("/repo+x", "x")]))
        with contextlib.redirect_stdout(io.StringIO()):
            self._wt(git, confirm=lambda p: False).remove("x")
        self.assertNotIn(["branch", "-D", "x"], git.commands)

    def test_interactive_confirm_deletes_branch(self):
        git = FakeGit(porcelain=make_porcelain([("/repo", "main"), ("/repo+x", "x")]))
        with contextlib.redirect_stdout(io.StringIO()):
            self._wt(git, confirm=lambda p: True).remove("x")
        self.assertIn(["branch", "-D", "x"], git.commands)

    def test_branch_delete_failure_raises(self):
        git = FakeGit(
            porcelain=make_porcelain([("/repo", "main"), ("/repo+x", "x")]),
            action_rc={("branch-D", "x"): 1},
        )
        with self.assertRaises(git_wt.GitWtError), contextlib.redirect_stdout(io.StringIO()):
            self._wt(git).remove("x", force_all=True)


# --------------------------------------------------------------------------------------
# Cleaner (orchestration)
# --------------------------------------------------------------------------------------


class TestCleaner(unittest.TestCase):
    def _cleaner(self, git, gh):
        resolver = git_wt.AccountResolver(git, gh, offline=False, identity=lambda r, c: "me")
        worktrees = git_wt.Worktrees(
            git, confirm=lambda p: True, path_in_use=lambda p: False, template=Path("/nonexistent")
        )
        return git_wt.Cleaner(git, resolver, gh, worktrees)

    def _scenario(
        self,
        *,
        current="feature",
        current_pr=None,
        counts=None,
        action_rc=None,
        statuses=None,
        contains=None,
    ):
        porcelain = make_porcelain([("/repo", current), ("/repo+merged", "merged-branch")])
        refs = make_refs(
            [
                ("main", "main-sha", ""),
                (current, f"{current}-sha", ""),
                ("merged-branch", "mb-sha", "[gone]"),
                ("open-branch", "ob-sha", "[ahead 1]"),
            ]
        )
        states = {
            "merged-branch": git_wt.PrInfo(10, "MERGED", "t"),
            "open-branch": git_wt.PrInfo(11, "OPEN", None),
        }
        if current_pr:
            merged_at = "t" if current_pr == "MERGED" else None
            states[current] = git_wt.PrInfo(12, current_pr, merged_at)
        git = FakeGit(
            porcelain=porcelain,
            refs=refs,
            statuses=statuses or {},
            default_head="main",
            counts=counts or {},
            action_rc=action_rc or {},
            contains=contains or {},
        )
        return git, FakeGh(states=states, active="me", accounts=("me",))

    def test_dry_run_makes_no_changes(self):
        git, gh = self._scenario()
        plan = self._cleaner(git, gh).run(
            apply=False,
            include_dirty=False,
            include_closed_local=False,
            reset_main=False,
            no_fetch=True,
        )
        self.assertEqual([c.branch for c in plan.tier_a], ["merged-branch"])
        self.assertEqual([c.branch for c in plan.kept], ["open-branch"])
        self.assertEqual(plan.reset.disposition, "relocate")
        self.assertFalse(git.issued("switch"))
        self.assertFalse(git.issued("branch", "-D"))

    def test_apply_deletes_tier_a_and_relocates(self):
        git, gh = self._scenario(counts={"main..origin/main": 5})
        with contextlib.redirect_stdout(io.StringIO()):
            plan = self._cleaner(git, gh).run(
                apply=True,
                include_dirty=False,
                include_closed_local=False,
                reset_main=True,
                no_fetch=True,
            )
        self.assertEqual(plan.tier_a[0].result, "removed")
        self.assertTrue(git.issued("worktree", "remove", "--force"))
        self.assertIn(["branch", "-D", "merged-branch"], git.commands)
        self.assertIn(["switch", "main"], git.commands)  # main reset to default
        self.assertTrue(git.issued("worktree", "add"))  # feature relocated
        self.assertEqual(plan.reset.result, "relocated")
        self.assertEqual(plan.sync.result, "fast-forwarded 5 commit(s)")
        self.assertIn(["worktree", "prune"], git.commands)

    def test_sync_up_to_date_label(self):
        git, gh = self._scenario(counts={"main..origin/main": 0})
        with contextlib.redirect_stdout(io.StringIO()):
            plan = self._cleaner(git, gh).run(
                apply=True,
                include_dirty=False,
                include_closed_local=False,
                reset_main=True,
                no_fetch=True,
            )
        self.assertEqual(plan.sync.result, "already up to date")

    def test_reset_delete_when_current_merged(self):
        # Current branch's PR is merged and its commits are recoverable -> delete after switch.
        git, gh = self._scenario(current_pr="MERGED", contains={"feature-sha": ["origin/feature"]})
        with contextlib.redirect_stdout(io.StringIO()):
            plan = self._cleaner(git, gh).run(
                apply=True,
                include_dirty=False,
                include_closed_local=False,
                reset_main=True,
                no_fetch=True,
            )
        self.assertEqual(plan.reset.disposition, "delete")
        self.assertIn(["switch", "main"], git.commands)
        self.assertIn(
            ["branch", "-D", "feature"], git.commands
        )  # current branch deleted after switch
        self.assertFalse(git.issued("worktree", "add"))

    def test_blocked_dirty_skips_reset(self):
        git, gh = self._scenario(statuses={"/repo": " M tracked.txt\n"})
        with contextlib.redirect_stdout(io.StringIO()):
            plan = self._cleaner(git, gh).run(
                apply=True,
                include_dirty=False,
                include_closed_local=False,
                reset_main=True,
                no_fetch=True,
            )
        self.assertEqual(plan.reset.disposition, "blocked-dirty")
        self.assertEqual(plan.reset.result, "blocked")
        self.assertFalse(git.issued("switch"))

    def test_gh_switch_and_restore_during_run(self):
        git = FakeGit(
            porcelain=make_porcelain([("/repo", "main")]),
            refs=make_refs([("main", "s", "")]),
            default_head="main",
        )
        gh = FakeGh(states={}, active="kriansa", accounts=("kriansa", "me"))
        resolver = git_wt.AccountResolver(git, gh, offline=False, identity=lambda r, c: "me")
        cleaner = git_wt.Cleaner(git, resolver, gh, git_wt.Worktrees(git))
        cleaner.run(
            apply=False,
            include_dirty=False,
            include_closed_local=False,
            reset_main=False,
            no_fetch=True,
        )
        self.assertEqual(gh.switches, ["me", "kriansa"])


# --------------------------------------------------------------------------------------
# Reporter
# --------------------------------------------------------------------------------------


class TestReporter(unittest.TestCase):
    def _plan(self):
        plan = git_wt.CleanupPlan(default="main", current="feature")
        plan.tier_a = [git_wt.Candidate("m", "/repo+m", "MERGED", 9, "merged #9", result="removed")]
        plan.reset = git_wt.ResetPlan("relocate", "feature", target="/repo+feature")
        plan.sync = git_wt.SyncStatus("synced", 0, 3, result="fast-forwarded 3 commit(s)")
        plan.ctx = git_wt.GhContext("gh", "acme/repo", "github.com", "me")
        return plan

    def test_json_shape(self):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            git_wt.Reporter(as_json=True).render(self._plan(), applied=True)
        data = json.loads(buf.getvalue())
        self.assertTrue(data["applied"])
        self.assertEqual(data["repo"], "acme/repo")
        self.assertEqual(data["tier_a"][0]["result"], "removed")
        self.assertEqual(data["reset_main"]["disposition"], "relocate")

    def test_human_layout_header_before_removals_before_sections(self):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            git_wt.Reporter(as_json=False).render(self._plan(), applied=True)
        out = buf.getvalue()
        # Header, then the grouped "Deleted worktree" lines, then the classified sections.
        self.assertLess(out.index("Repo: acme/repo"), out.index("Deleted worktree repo+m"))
        self.assertLess(out.index("Deleted worktree repo+m"), out.index("Merged - 1:"))

    def test_human_output_uses_renamed_titles(self):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            git_wt.Reporter(as_json=False).render(self._plan(), applied=True)
        out = buf.getvalue()
        self.assertIn("Merged - 1:", out)
        self.assertIn("Dirty worktree (--discard-local-changes) - 0:", out)
        self.assertIn("Closed (--include-closed) - 0:", out)
        self.assertIn("Reset main worktree (--reset-main):", out)
        self.assertIn("fast-forwarded 3 commit(s)", out)
        self.assertIn("Kept - 0:", out)


# --------------------------------------------------------------------------------------
# Cli
# --------------------------------------------------------------------------------------


class TestSpinner(unittest.TestCase):
    def test_disabled_is_noop_context(self):
        with git_wt.Spinner("working", enabled=False) as spinner:
            self.assertIsInstance(spinner, git_wt.Spinner)
        self.assertIsNone(spinner._thread)  # never started a thread

    def test_factory_produces_context_manager(self):
        with git_wt.make_spinner(enabled=False)("working"):
            pass  # must not raise


class TestCli(unittest.TestCase):
    def parse(self, argv):
        return git_wt.Cli.build_parser().parse_args(argv)

    def test_no_subcommand_defaults_to_list(self):
        self.assertIs(self.parse([]).run, git_wt.Cli.cmd_list)

    def test_add_parsing(self):
        args = self.parse(["add", "-b", "feat/x"])
        self.assertIs(args.run, git_wt.Cli.cmd_add)
        self.assertTrue(args.create)
        self.assertEqual(args.branch, "feat/x")

    def test_rm_aliases_and_force_all(self):
        args = self.parse(["delete", "-F", "x"])
        self.assertIs(args.run, git_wt.Cli.cmd_rm)
        self.assertTrue(args.force_all)

    def test_gc_flags(self):
        args = self.parse(["gc", "--apply", "--reset-main", "--json"])
        self.assertIs(args.run, git_wt.Cli.cmd_gc)
        self.assertTrue(args.apply and args.reset_main and args.as_json)

    def test_gc_renamed_flags_map_to_dests(self):
        args = self.parse(["gc", "--discard-local-changes", "--include-closed"])
        self.assertTrue(args.include_dirty)  # --discard-local-changes
        self.assertTrue(args.include_closed_local)  # --include-closed

    def test_cmd_list_prints_worktrees(self):
        git = FakeGit(porcelain="WORKTREE LISTING\n")
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            git_wt.Cli.cmd_list(self.parse(["list"]), git)
        self.assertIn("WORKTREE LISTING", buf.getvalue())


if __name__ == "__main__":
    unittest.main()
