# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal dotfiles for macOS and Arch/Fedora Linux. The shell is [Fish](https://fishshell.com/). Two independent layers:

- **`dotup`** (`bin/dotup`) — a custom symlink/stow manager that links configuration from `modules/` into `$HOME`. This is the everyday tool.
- **Ansible** (`ansible/`) — provisions a bare machine (packages, drivers, boot, services). Run once per machine, not for routine config changes.

## Commands

```sh
dotup                       # apply/reload all enabled modules' links (alias: bin/dotup load)
dotup unload                # remove every link dotup created (tracked in dotstate.db)
dotup -s work               # apply only modules whose scopes ⊆ {work}; scopes stack
dotup is-enabled <module>   # exit 0 if module is enabled in current scope (used by scripts)

bin/setup <[user@]host> <machine-type> [--debug]   # wraps ansible-playbook; machine-type ∈ {laptop, desktop, laptop_twm}
                                                    # host=localhost runs locally; --debug runs only tasks tagged `only`
update-system               # OS + language-toolchain + fish-completion updates (brew/pacman/dnf aware)
```

Python tooling (for `bin/dotup` and `*.py` modules/plugins): format with `black` (line-length 100, see `pyproject.toml`), type-check with `mypy`. There is no test suite.

## Architecture: dotup + modules

`dotup` reads `dotfiles.toml` (the ordered module list) and, for each enabled module under `modules/<name>/`, creates links by **convention based on directory**:

| Source in module | Linked to | Notes |
|---|---|---|
| `include/<path>` | `~/<path>` | `include/` mirrors `$HOME`; e.g. `include/.config/foo` → `~/.config/foo` |
| `bin/<file>` | `~/.bin/<file>` | on `$PATH` via fish `conf.d` |
| `functions/*.fish` | fish autoloaded functions | |
| `completions/*.fish` | fish completions | |
| `init.fish` | `conf.d/module_<name>_init.fish` | sourced at every shell startup |

Key behaviors of the linker (`bin/dotup`, the `Stower` class):

- **Directories are symlinked whole**, except paths in `paths_denylist()` (`.config`, `.claude`, `.claude/skills`, XDG dirs, etc.). Denylisted dirs are *descended into* and their files/children linked individually — this is how **multiple modules contribute into one shared directory** (e.g. both `ai` and `git` add skills under `~/.claude/skills`). If you add a top-level file to a shared dir like `~/.claude`, add its parent to the denylist or it won't merge.
- **State** lives in `dotstate.db` (SQLite, gitignored, machine-local). `unload` only removes links recorded there and pointing back into the repo — it never touches foreign files.
- **Conflicts are surfaced, not clobbered**: linking refuses if the destination exists with different content (identical content is silently replaced with the link).
- A few paths are **hardlinked** instead of symlinked (`*.desktop`, `mimeapps.list`) — see `paths_hardlink()`.
- `bin/dotup` is **stdlib-only** Python; on Python < 3.11 it falls back to the vendored `pip._vendor.tomli` for TOML.

### Scopes

Modules in `dotfiles.toml` may be bare strings or `{ name, scopes = [...] }`. A scoped module loads only when the active scopes are a **subset** of its declared scopes. Active scopes come from `-s` flags or the `.dotup-scopes` file (gitignored, machine-local; e.g. `work`, `personal`, `devbox`). Bare modules always load.

### Editing workflow (meta module fish functions)

- `dotadd <PATH> <MODULE>` — move a real file from `$HOME` into `modules/<MODULE>/include/` and symlink it back. This is how new config enters the repo.
- `dote` — open the repo in `$EDITOR`; `dotcd` — cd into it; `dotrm` — remove a tracked file.

After changing any module, run `dotup` to re-link.

## Ansible bootstrap

`ansible/` provisions machines. Playbooks (`laptop.yml`, `desktop.yml`, `laptop_twm.yml`) set host vars and compose `roles/` (base-arch, systemd-boot, dev-tools, network, kde, yubikey, borgmatic, …). `bin/setup` is the only intended entry point. See `doc/arch-install.md` / `doc/macos-install.md` for the manual pre-steps before Ansible/`dotup` can run.

## Conventions

- **`.editorconfig`**: utf-8, LF, final newline, trim trailing whitespace, max line 100. Indent style/size is deliberately *not* set globally (EditorConfig can't do per-language) — it's set per-filetype in Neovim ftplugins (`modules/nvim/.../settings.lua`). Match the indentation already in a file.
- Prefer Fish for shell logic; bash only where portability is required (e.g. `bin/setup`, `update-system`).

## Working preferences

Carried from persistent memory — apply these proactively:

- **Concise code comments.** State only *what* the code does and *why*. Omit exploration history, alternatives considered, and session-discovered gotchas — comments are read later with no session context.
- **Vendored, dependency-free tooling.** Strongly prefer self-contained tools with zero third-party runtime deps over anything that fetches from a package registry at runtime (`npx pkg@latest`, marketplace skills) — explicitly to eliminate supply-chain attack surface. When adding/replacing a tool or Claude skill: use only the standard library (Python stdlib + `ctypes`/`subprocess` for OS integration); vendor it under `modules/<m>/include/.claude/skills/<name>/` so `dotup` links it into `~/.claude/skills/` (mirror the `git-worktree` skill); run single-file Python tools via `uv` with PEP 723 inline metadata (`# /// script`). If a dependency seems unavoidable, surface the trade-off and ask first.
