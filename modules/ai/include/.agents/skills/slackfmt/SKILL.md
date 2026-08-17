---
name: slackfmt
description: Copy or format text for Slack with rich formatting preserved. Use this whenever the user wants to copy something to Slack, paste into Slack, send a message to Slack, or format or prepare any text or message for Slack — phrases like copy this to Slack, copy for Slack, format for Slack, paste into Slack, make it Slack-ready, or slackify this. Converts markdown to Slack rich text on the clipboard so bold, italic, lists, code blocks, links, and quotes survive the paste.
---

# slackfmt

Convert markdown to Slack's native rich text format and copy to clipboard. When pasted into Slack, all formatting (bold, italic, lists, code blocks, links, blockquotes) is preserved.

This is a local, dependency-free tool (`slackfmt`, next to this file) — it does **not** fetch anything from a package registry at runtime. The script is directly executable via its shebang (`uv run --script`), so invoke it as a normal command.

## When to Use

Use this skill whenever Slack and copying/formatting/pasting are both in play. Triggers include:

- "copy this to Slack" / "copy this for Slack" / "copy to Slack"
- "format this for Slack" / "make this Slack-ready" / "slackify this"
- "paste into Slack" / "send this to Slack"
- The user has markdown (or a message/summary) that is destined for Slack and wants formatting preserved

**Slack limitations:** Headings are converted to bold text. Only basic formatting is supported (bold, italic, strikethrough, code, links, lists, blockquotes, code blocks).

## Requirements

- `uv` must be installed — the script bootstraps its own (empty) environment via its shebang (`uv run --script`).
- On Linux, a clipboard tool that can set custom MIME types: `wl-copy` (Wayland) or `xclip` (X11). macOS needs nothing extra (uses the system pasteboard via the Objective-C runtime).

## CLI Usage

```bash
# Pipe markdown to slackfmt (copies to clipboard by default)
echo "**bold** and _italic_" | ~/.claude/skills/slackfmt/slackfmt
```

**Flags:**

- `--stdout` — Print the Quill Delta JSON to stdout instead of copying to clipboard (useful for inspection/debugging).

## Instructions

1. Compose the content as markdown in an `echo` or `printf` command
2. Pipe it to `~/.claude/skills/slackfmt/slackfmt`
3. The formatted content is copied to the system clipboard
4. User can paste directly into Slack with formatting preserved

### Example

```bash
echo '- **Performance**
  - Dashboard speed improvements
  - 90% faster queries
- **Bug Fixes**
  - Fixed mobile layout issue' | ~/.claude/skills/slackfmt/slackfmt
```

### Tips

- Use `*` or `**` for bold, `_` or `*` for italic (standard markdown)
- Nested lists work — use 2-space indentation
- Links: `[text](url)` becomes clickable in Slack
- Code blocks: use triple backticks
- Blockquotes: use `>`
- Headings (`#`, `##`, etc.) are converted to bold paragraphs

### Linux note

On Linux only the `org.chromium.web-custom-data` clipboard flavor (the one Slack reads) is published; a coexisting plain-text flavor is not guaranteed, because `wl-copy`/`xclip` own the selection per invocation. On macOS both the rich flavor and the raw markdown (plain text) are set.
