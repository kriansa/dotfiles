#!/usr/bin/env bash
# Claude Code notification hook

msg=$(cat - | jq -r '.message // "Claude Code needs your attention"')

case "$(uname)" in
  Linux)
    # TODO: check if ghostty is focused before showing notification
    notify-send -a "Claude Code" --hint=string:desktop-entry:com.mitchellh.ghostty --app-icon=~/.claude/icon.png "$msg"
    ;;
  Darwin)
    focused=$(osascript -e 'tell application "System Events" to get bundle identifier of first process whose frontmost is true')
    if [[ "$focused" == "com.mitchellh.ghostty" ]]; then
      exit 0
    fi
    hs -c "hs.alert.show('Claude Code: $msg')"
    afplay /System/Library/Sounds/Submarine.aiff &
    ;;
esac
