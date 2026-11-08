# External monitor brightness watchdog

PowerDevil sometimes loses DDC/CI brightness control of an external monitor after it wakes up from
standby, and never retries. `powerdevil-ddc-hotplug.service` runs `powerdevil-ddc-check.service` on
every monitor plug/unplug, which restarts PowerDevil only when a monitor answers DDC/CI but Plasma
can't control its brightness. Ansible's `kde` role enables it; to do it by hand:

```bash
systemctl --user enable --now powerdevil-ddc-hotplug.service
```

Check what it did with `journalctl --user -u powerdevil-ddc-check`.

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
