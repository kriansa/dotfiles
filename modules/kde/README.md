# External monitor brightness watchdog

PowerDevil sometimes loses DDC/CI brightness control of an external monitor after it wakes up from
standby, and never retries. `powerdevil-ddc-hotplug.service` runs `powerdevil-ddc-check.service` on
every monitor plug/unplug, which restarts PowerDevil only when a monitor answers DDC/CI but Plasma
can't control its brightness. Ansible's `kde` role enables it; to do it by hand:

```bash
systemctl --user enable --now powerdevil-ddc-hotplug.service
```

Check what it did with `journalctl --user -u powerdevil-ddc-check`.

# Tile memory

The `tile-memory` KWin script puts a window back into the custom tile (the Meta+T layouts) it was
last in when it opens again. It only acts on windows matched by a window rule whose description
contains `[tile-memory]`; add the tag in System Settings → Window Management → Window Rules.

A remembered tile is skipped when the window opens on another virtual desktop, the monitor isn't
connected, the layout's shape (its splits and tile count, not their sizes) has changed, or a
matching rule forces the window's position or size. The memory lives in
`~/.local/state/tile-memory/state.ini`.

KWin keeps a script's code for the whole session, even if the script is disabled and enabled
again, so edits to it only take effect after logging in again.

# Enable system76-scheduler integration

## Status

Currently this is disabled. `system76-scheduler` doesn't seem to properly reassign priorities to
processes on foreground an background.

## Installation

Run the following commands to enable system76-scheduler integration:

```bash
cp -a kwin-plugins/system76-scheduler ~/.local/share/kwin/scripts/
kwriteconfig5 --file kwinrc --group Plugins --key system76-schedulerEnabled true
qdbus org.kde.KWin /KWin reconfigure
systemctl --user enable --now system76-scheduler-dbus-proxy
```
