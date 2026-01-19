#!/usr/bin/env bash
# Claude Code notification hook

msg=$(cat - | jq -r '.message // "Claude Code needs your attention"')

case "$(uname)" in
  Linux)
    notify-send -a "Claude Code" --hint=string:desktop-entry:com.mitchellh.ghostty --app-icon=~/.claude/icon.png "$msg"
    ;;
  Darwin)
    hs -c "hs.alert.show('Claude Code: $msg')"
    ;;
esac
