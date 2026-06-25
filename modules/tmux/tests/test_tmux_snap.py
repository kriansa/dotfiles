"""Unit tests for the tmux-snap tool (stdlib unittest, no third-party deps).

The tool is an extension-less PEP 723 uv script, so it is loaded as a module by
path. Its top level only defines things (guarded by __main__), so importing is safe.
Run: python3 -m unittest discover -s modules/tmux/tests -t modules/tmux/tests -v
"""

# The tool is loaded dynamically by path, so its symbols (e.g. tmux_snap.Tmux) are not
# statically resolvable by mypy; types are exercised at runtime instead.
# mypy: ignore-errors

import importlib.machinery
import importlib.util
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

# Don't write a .pyc into modules/tmux/bin/__pycache__ — dotup links everything under a
# module's bin/, and that cache dir is not a tool to put on PATH.
sys.dont_write_bytecode = True

_TOOL = Path(__file__).resolve().parents[1] / "bin" / "tmux-snap"
_loader = importlib.machinery.SourceFileLoader("tmux_snap", str(_TOOL))
_spec = importlib.util.spec_from_loader("tmux_snap", _loader)
assert _spec is not None
tmux_snap = importlib.util.module_from_spec(_spec)
_loader.exec_module(tmux_snap)

US = tmux_snap.Tmux.FIELD_SEPARATOR


# --------------------------------------------------------------------------- #
# Fakes
# --------------------------------------------------------------------------- #


class FakeTmux(tmux_snap.Tmux):
    """Records issued commands and returns canned output, without touching tmux."""

    def __init__(
        self,
        *,
        existing_sessions=(),
        windows_output="",
        panes_output=None,
        pane_indexes=None,
        window_indexes=(1, 2, 3, 4),
        session_name="sess",
        socket_args=None,
        from_tmux_env=False,
    ):
        super().__init__(list(socket_args or []), from_tmux_env=from_tmux_env)
        self.existing = set(existing_sessions)
        self.windows_output = windows_output
        self.panes_output = panes_output or {}
        self.pane_index_map = pane_indexes or {}
        self.window_indexes = list(window_indexes)
        self.session_name = session_name
        self.commands: list[list[str]] = []

    def _ok(self, args):
        self.commands.append(args)
        if args[0] == "has-session":
            return args[2].removeprefix("=") in self.existing
        return True

    def _run(self, args, *, capture=True):
        self.commands.append(args)
        sub = args[0]
        if sub == "display-message":
            return self.session_name + "\n"
        if sub == "list-windows":
            return self.windows_output
        if sub == "list-panes":
            target, fmt = args[2], args[4]
            if fmt == "#{pane_index}":
                return "\n".join(str(i) for i in self.pane_index_map.get(target, []))
            return self.panes_output.get(target, "")
        if sub in ("new-session", "new-window"):
            return str(self.window_indexes.pop(0))
        return ""

    def issued(self, subcommand):
        return [cmd for cmd in self.commands if cmd and cmd[0] == subcommand]

    def first_index(self, predicate):
        return next((i for i, cmd in enumerate(self.commands) if predicate(cmd)), -1)


class FakeClaudeRegistry:
    def __init__(self, result):
        self.result = result
        self.calls = []

    def lookup(self, foreground_pid, cwd):
        self.calls.append((foreground_pid, cwd))
        return self.result


def win_row(index, name, active, layout, zoomed):
    return US.join([str(index), name, active, layout, zoomed])


def pane_row(index, active, sync, cwd, command, pid):
    return US.join([str(index), active, sync, cwd, command, str(pid)])


def make_handlers(claude_result=None):
    registry = FakeClaudeRegistry(claude_result or tmux_snap.ClaudeSession(None, None))
    return tmux_snap.HandlerRegistry(
        [
            tmux_snap.ClaudeHandler(registry),
            tmux_snap.CommandHandler("nvim"),
            tmux_snap.CommandHandler("vim"),
        ]
    )


def pane(index=1, *, active=True, cwd="/proj", restore=None):
    return tmux_snap.Pane(
        index=index,
        is_active=active,
        cwd=cwd,
        shell_pid=index * 100,
        current_command="x",
        restore=restore,
    )


# --------------------------------------------------------------------------- #
# Pure logic
# --------------------------------------------------------------------------- #


class TestProcessTree(unittest.TestCase):
    def test_shell_to_app(self):
        tree = tmux_snap.ProcessTree.from_ps_output("100 1 -fish\n101 100 claude --effort max\n")
        fg = tree.foreground(100)
        self.assertEqual(
            (fg.pid, fg.command, fg.argv), (101, "claude", ["claude", "--effort", "max"])
        )

    def test_bare_shell(self):
        tree = tmux_snap.ProcessTree.from_ps_output("100 1 -zsh\n")
        fg = tree.foreground(100)
        self.assertEqual((fg.pid, fg.command, fg.argv), (None, "zsh", []))

    def test_multi_child_takes_deepest_highest_pid(self):
        tree = tmux_snap.ProcessTree.from_ps_output(
            "10 1 -fish\n11 10 vim\n15 10 nvim\n20 15 fzf\n"
        )
        fg = tree.foreground(10)
        self.assertEqual((fg.pid, fg.command), (20, "fzf"))

    def test_cycle_guard_terminates(self):
        tree = tmux_snap.ProcessTree({1: [2], 2: [1]}, {1: "a", 2: "b"})
        fg = tree.foreground(1)  # must not loop forever
        self.assertIsNotNone(fg.pid)

    def test_command_basename(self):
        self.assertEqual(tmux_snap.ProcessTree._command_basename("/usr/bin/nvim"), "nvim")
        self.assertEqual(tmux_snap.ProcessTree._command_basename("-zsh"), "zsh")
        self.assertEqual(tmux_snap.ProcessTree._command_basename("claude --effort max"), "claude")
        self.assertEqual(tmux_snap.ProcessTree._command_basename(""), "")


class TestPreservedClaudeFlags(unittest.TestCase):
    def test_keeps_effort_and_model(self):
        self.assertEqual(
            tmux_snap.ClaudeHandler.preserved_claude_flags(
                ["claude", "--effort", "max", "--model", "opus"]
            ),
            ["--effort", "max", "--model", "opus"],
        )

    def test_equals_form(self):
        self.assertEqual(
            tmux_snap.ClaudeHandler.preserved_claude_flags(["claude", "--effort=high"]),
            ["--effort=high"],
        )

    def test_drops_resume_and_dangerous(self):
        argv = [
            "claude",
            "--resume",
            "abc",
            "--allow-dangerously-skip-permissions",
            "--effort",
            "max",
        ]
        self.assertEqual(tmux_snap.ClaudeHandler.preserved_claude_flags(argv), ["--effort", "max"])

    def test_empty(self):
        self.assertEqual(tmux_snap.ClaudeHandler.preserved_claude_flags(["claude"]), [])


class TestClaudeRegistry(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(self.enterContext(tempfile.TemporaryDirectory()))
        self.registry = tmux_snap.ClaudeRegistry(self.tmp)

    def _write(self, pid, **fields):
        (self.tmp / f"{pid}.json").write_text(json.dumps(fields))

    def test_lookup_by_pid(self):
        self._write(42, sessionId="SID", name="work", cwd="/proj")
        session = self.registry.lookup(42, "/proj")
        self.assertEqual((session.session_id, session.name), ("SID", "work"))

    def test_cwd_fallback_picks_newest(self):
        self._write(1, sessionId="old", cwd="/proj")
        self._write(2, sessionId="new", cwd="/proj")
        os.utime(self.tmp / "1.json", (1000, 1000))
        os.utime(self.tmp / "2.json", (2000, 2000))
        self.assertEqual(self.registry.lookup(999, "/proj").session_id, "new")

    def test_missing_returns_none(self):
        session = self.registry.lookup(7, "/nowhere")
        self.assertEqual((session.session_id, session.name), (None, None))

    def test_malformed_is_skipped(self):
        (self.tmp / "9.json").write_text("{not json")
        self.assertIsNone(self.registry.lookup(9, "/proj").session_id)


class TestHandlers(unittest.TestCase):
    def test_command_handler(self):
        self.assertEqual(tmux_snap.CommandHandler("nvim").build_command({}), "nvim")

    def test_claude_build_command_session_id(self):
        handler = tmux_snap.ClaudeHandler(FakeClaudeRegistry(tmux_snap.ClaudeSession(None, None)))
        state = {"session_id": "X", "name": None, "flags": ["--effort", "max"]}
        self.assertEqual(handler.build_command(state), "claude --resume X --effort max")

    def test_claude_build_command_name_fallback_is_quoted(self):
        handler = tmux_snap.ClaudeHandler(FakeClaudeRegistry(tmux_snap.ClaudeSession(None, None)))
        self.assertEqual(
            handler.build_command({"session_id": None, "name": "my sess", "flags": []}),
            "claude --resume 'my sess'",
        )

    def test_claude_build_command_bare(self):
        handler = tmux_snap.ClaudeHandler(FakeClaudeRegistry(tmux_snap.ClaudeSession(None, None)))
        self.assertEqual(
            handler.build_command({"session_id": None, "name": None, "flags": []}), "claude"
        )

    def test_claude_capture_uses_registry(self):
        registry = FakeClaudeRegistry(tmux_snap.ClaudeSession("S", None))
        handler = tmux_snap.ClaudeHandler(registry)
        context = tmux_snap.PaneContext(101, "/proj", "claude", ["claude", "--effort", "max"])
        self.assertEqual(
            handler.capture(context),
            {"session_id": "S", "name": None, "flags": ["--effort", "max"]},
        )
        self.assertEqual(registry.calls, [(101, "/proj")])

    def test_registry_match_and_by_id(self):
        handlers = make_handlers()
        self.assertIsInstance(handlers.match("claude", []), tmux_snap.ClaudeHandler)
        self.assertIsInstance(handlers.match("nvim", []), tmux_snap.CommandHandler)
        self.assertIsNone(handlers.match("zsh", []))
        self.assertIsNotNone(handlers.by_id("vim"))
        self.assertIsNone(handlers.by_id("ghost"))

    def test_registry_coerces_string_to_command_handler(self):
        handler = tmux_snap.HandlerRegistry(["nvim"]).by_id("nvim")
        self.assertIsInstance(handler, tmux_snap.CommandHandler)
        self.assertEqual(handler.build_command({}), "nvim")


class TestSnapshotModel(unittest.TestCase):
    def test_round_trip(self):
        snap = tmux_snap.Snapshot(
            schema_version=1,
            saved_at="2026-01-01T00:00:00+00:00",
            session_name="sess",
            active_window_index=2,
            windows=[
                tmux_snap.Window(
                    index=2,
                    name="work",
                    is_active=True,
                    layout="L",
                    is_zoomed=True,
                    is_synchronized=True,
                    panes=[
                        pane(
                            1,
                            restore=tmux_snap.RestoreAction(
                                "claude", {"session_id": "X", "flags": []}
                            ),
                        ),
                        pane(2, active=False, cwd="/tmp", restore=None),
                    ],
                )
            ],
        )
        self.assertEqual(tmux_snap.Snapshot.from_dict(json.loads(json.dumps(snap.to_dict()))), snap)

    def test_bad_schema_version_raises(self):
        with self.assertRaises(tmux_snap.TmuxSnapError):
            tmux_snap.Snapshot.from_dict({"schema_version": 99, "windows": []})


class TestSnapshotStore(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(self.enterContext(tempfile.TemporaryDirectory()))
        self.store = tmux_snap.SnapshotStore(self.tmp)

    def _snapshot(self, name="sess"):
        return tmux_snap.Snapshot(
            1, "t", name, 1, [tmux_snap.Window(1, "w", True, "L", False, False, [pane()])]
        )

    def test_save_load_round_trip(self):
        self.store.save(self._snapshot(), "demo")
        self.assertEqual(self.store.load("demo"), self._snapshot())

    def test_invalid_names_rejected(self):
        for bad in ("a/b", "..", ""):
            with self.assertRaises(tmux_snap.TmuxSnapError):
                self.store.path(bad)

    def test_entries_sorted_newest_first(self):
        self.store.save(self._snapshot(), "old")
        self.store.save(self._snapshot(), "new")
        os.utime(self.tmp / "old.json", (1000, 1000))
        os.utime(self.tmp / "new.json", (2000, 2000))
        self.assertEqual([entry.name for entry in self.store.entries()], ["new", "old"])

    def test_entries_marks_unreadable(self):
        (self.tmp / "broken.json").write_text("{garbage")
        (info,) = self.store.entries()
        self.assertFalse(info.readable)
        self.assertIn("unreadable", info.describe())

    def test_destroy_and_missing(self):
        self.store.save(self._snapshot(), "demo")
        self.store.destroy("demo")
        with self.assertRaises(tmux_snap.TmuxSnapError):
            self.store.destroy("demo")

    def test_entries_missing_dir(self):
        self.assertEqual(tmux_snap.SnapshotStore(self.tmp / "nope").entries(), [])


class TestResolveSocket(unittest.TestCase):
    def test_socket_name(self):
        tmux = tmux_snap.Tmux.from_args(socket_name="snap", socket_path=None)
        self.assertEqual((tmux.socket_args, tmux.from_tmux_env), (["-L", "snap"], False))

    def test_socket_path(self):
        tmux = tmux_snap.Tmux.from_args(socket_name=None, socket_path="/p")
        self.assertEqual(tmux.socket_args, ["-S", "/p"])

    def test_tmux_env(self):
        with mock.patch.dict(os.environ, {"TMUX": "/tmp/sock,9,0"}, clear=True):
            tmux = tmux_snap.Tmux.from_args(socket_name=None, socket_path=None)
        self.assertEqual((tmux.socket_args, tmux.from_tmux_env), (["-S", "/tmp/sock"], True))

    def test_default(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            tmux = tmux_snap.Tmux.from_args(socket_name=None, socket_path=None)
        self.assertEqual((tmux.socket_args, tmux.from_tmux_env), ([], False))


# --------------------------------------------------------------------------- #
# Capture / restore orchestration (against FakeTmux)
# --------------------------------------------------------------------------- #


class TestCapture(unittest.TestCase):
    def _snapshotter(self, tmux):
        ps = "2000 1 -fish\n2001 2000 nvim\n1000 1 -fish\n1001 1000 claude --effort max\n3000 1 -zsh\n"
        handlers = make_handlers(claude_result=tmux_snap.ClaudeSession("SID-123", None))
        return tmux_snap.Snapshotter(
            tmux, handlers, lambda: tmux_snap.ProcessTree.from_ps_output(ps)
        )

    def test_capture_models_session(self):
        tmux = FakeTmux(
            existing_sessions={"sess"},
            windows_output="\n".join(
                [win_row(1, "editor", "0", "L1", "0"), win_row(2, "work", "1", "L2", "0")]
            ),
            panes_output={
                "sess:1": pane_row(1, "1", "0", "/proj", "nvim", 2000),
                "sess:2": "\n".join(
                    [
                        pane_row(1, "1", "1", "/proj", "claude", 1000),
                        pane_row(2, "0", "1", "/tmp", "zsh", 3000),
                    ]
                ),
            },
        )
        snapshot = self._snapshotter(tmux).capture("sess")

        self.assertEqual(snapshot.active_window_index, 2)
        editor, work = snapshot.windows
        self.assertEqual(editor.panes[0].restore.handler_id, "nvim")
        self.assertEqual(editor.panes[0].restore.state, {})
        self.assertTrue(work.is_synchronized)
        claude_pane, plain_pane = work.panes
        self.assertEqual(claude_pane.restore.handler_id, "claude")
        self.assertEqual(claude_pane.restore.state["session_id"], "SID-123")
        self.assertEqual(claude_pane.restore.state["flags"], ["--effort", "max"])
        self.assertIsNone(plain_pane.restore)

    def test_capture_unknown_session_raises(self):
        with self.assertRaises(tmux_snap.TmuxSnapError):
            self._snapshotter(FakeTmux(existing_sessions=set())).capture("ghost")


class TestRestore(unittest.TestCase):
    def _snapshot(self, *, sync=True, panes2_restore=None):
        if panes2_restore is None:
            panes2_restore = tmux_snap.RestoreAction(
                "claude", {"session_id": "X", "name": None, "flags": []}
            )
        return tmux_snap.Snapshot(
            1,
            "t",
            "src",
            2,
            [
                tmux_snap.Window(
                    1,
                    "editor",
                    False,
                    "L1",
                    False,
                    False,
                    [pane(1, restore=tmux_snap.RestoreAction("nvim", {}))],
                ),
                tmux_snap.Window(
                    2,
                    "work",
                    True,
                    "L2",
                    False,
                    sync,
                    [
                        pane(1, restore=panes2_restore),
                        pane(2, active=False, cwd="/tmp", restore=None),
                    ],
                ),
            ],
        )

    def _fake(self, **kw):
        return FakeTmux(window_indexes=[1, 2], pane_indexes={"T:1": [1], "T:2": [1, 2]}, **kw)

    def test_build_issues_expected_sequence(self):
        tmux = self._fake()
        handlers = make_handlers()
        tmux_snap.Snapshotter(tmux, handlers, lambda: None).build(
            self._snapshot(), name="T", replace=False
        )

        self.assertEqual(len(tmux.issued("new-session")), 1)
        self.assertEqual(len(tmux.issued("new-window")), 1)
        self.assertEqual(len(tmux.issued("split-window")), 1)  # window 2's second pane
        # claude relaunch sent to pane 1 of window 2; plain pane gets nothing
        sends = tmux.issued("send-keys")
        self.assertTrue(
            any(cmd[2] == "T:2.1" and cmd[4].startswith("claude --resume X") for cmd in sends)
        )
        self.assertFalse(any(cmd[2] == "T:2.2" for cmd in sends))
        # active window selected
        self.assertTrue(any(cmd == ["select-window", "-t", "T:2"] for cmd in tmux.commands))

    def test_synchronize_enabled_after_send_keys(self):
        tmux = self._fake()
        tmux_snap.Snapshotter(tmux, make_handlers(), lambda: None).build(
            self._snapshot(sync=True), name="T", replace=False
        )
        send_index = tmux.first_index(lambda c: c[0] == "send-keys" and c[2] == "T:2.1")
        sync_index = tmux.first_index(
            lambda c: c[0] == "set-window-option" and "synchronize-panes" in c
        )
        self.assertGreater(sync_index, send_index)

    def test_collision_requires_replace(self):
        tmux = self._fake(existing_sessions={"T"})
        snapshotter = tmux_snap.Snapshotter(tmux, make_handlers(), lambda: None)
        with self.assertRaises(tmux_snap.TmuxSnapError):
            snapshotter.build(self._snapshot(), name="T", replace=False)

    def test_replace_kills_first(self):
        tmux = self._fake(existing_sessions={"T"})
        tmux_snap.Snapshotter(tmux, make_handlers(), lambda: None).build(
            self._snapshot(), name="T", replace=True
        )
        self.assertEqual(len(tmux.issued("kill-session")), 1)

    def test_unknown_handler_warns_and_skips(self):
        snapshot = self._snapshot(panes2_restore=tmux_snap.RestoreAction("ghost", {}))
        tmux = self._fake()
        snapshotter = tmux_snap.Snapshotter(tmux, make_handlers(), lambda: None)
        with self.assertLogs("tmux-snap", level="WARNING") as captured:
            snapshotter.build(snapshot, name="T", replace=False)
        self.assertTrue(any("unknown handler" in line for line in captured.output))
        self.assertFalse(any(cmd[0] == "send-keys" and cmd[2] == "T:2.1" for cmd in tmux.commands))


class TestAttach(unittest.TestCase):
    def test_inside_same_server_switches(self):
        tmux = FakeTmux(from_tmux_env=True)
        with mock.patch.dict(os.environ, {"TMUX": "/s,1,0"}, clear=True):
            tmux_snap.Cli._attach(tmux, "T", no_attach=False)
        self.assertEqual(len(tmux.issued("switch-client")), 1)
        self.assertEqual(len(tmux.issued("attach-session")), 0)

    def test_outside_attaches(self):
        tmux = FakeTmux()
        with mock.patch.dict(os.environ, {}, clear=True):
            tmux_snap.Cli._attach(tmux, "T", no_attach=False)
        self.assertEqual(len(tmux.issued("attach-session")), 1)

    def test_no_attach_inside_hints_switch(self):
        tmux = FakeTmux(from_tmux_env=True)
        with mock.patch.dict(os.environ, {"TMUX": "/s,1,0"}, clear=True):
            with self.assertLogs("tmux-snap", level="INFO") as captured:
                tmux_snap.Cli._attach(tmux, "T", no_attach=True)
        self.assertEqual(len(tmux.issued("switch-client")), 0)
        self.assertTrue(any("switch-client -t T" in line for line in captured.output))

    def test_different_server_hint_includes_socket(self):
        tmux = FakeTmux(socket_args=["-L", "snap"])  # explicit socket => different server
        with mock.patch.dict(os.environ, {"TMUX": "/s,1,0"}, clear=True):
            with self.assertLogs("tmux-snap", level="INFO") as captured:
                tmux_snap.Cli._attach(tmux, "T", no_attach=False)
        self.assertEqual(len(tmux.issued("attach-session")), 0)
        self.assertEqual(len(tmux.issued("switch-client")), 0)
        self.assertTrue(any("tmux -L snap attach -t T" in line for line in captured.output))


class TestCli(unittest.TestCase):
    def test_parser_save_dispatch(self):
        args = tmux_snap.Cli.build_parser().parse_args(
            ["-L", "snap", "save", "demo", "--session", "src"]
        )
        self.assertEqual((args.socket_name, args.name, args.session), ("snap", "demo", "src"))
        self.assertIs(args.run, tmux_snap.Cli.cmd_save)

    def test_parser_restore_flags(self):
        args = tmux_snap.Cli.build_parser().parse_args(
            ["restore", "demo", "--no-attach", "--replace"]
        )
        self.assertTrue(args.no_attach)
        self.assertTrue(args.replace)
        self.assertIs(args.run, tmux_snap.Cli.cmd_restore)

    def test_parser_restore_name_optional(self):
        self.assertIsNone(tmux_snap.Cli.build_parser().parse_args(["restore"]).name)

    def test_current_session_name_inside(self):
        args = tmux_snap.Cli.build_parser().parse_args(["save"])
        with mock.patch.dict(os.environ, {"TMUX": "/s,1,0"}, clear=True):
            self.assertEqual(
                tmux_snap.Cli._current_session_name(args, FakeTmux(session_name="work")), "work"
            )

    def test_current_session_name_detached_is_none(self):
        args = tmux_snap.Cli.build_parser().parse_args(["save"])
        with mock.patch.dict(os.environ, {}, clear=True):
            self.assertIsNone(tmux_snap.Cli._current_session_name(args, FakeTmux()))

    def test_current_session_name_explicit_socket_is_none(self):
        args = tmux_snap.Cli.build_parser().parse_args(["-L", "snap", "save"])
        with mock.patch.dict(os.environ, {"TMUX": "/s,1,0"}, clear=True):
            tmux = FakeTmux(socket_args=["-L", "snap"])
            self.assertIsNone(tmux_snap.Cli._current_session_name(args, tmux))


if __name__ == "__main__":
    unittest.main()
