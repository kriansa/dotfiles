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
