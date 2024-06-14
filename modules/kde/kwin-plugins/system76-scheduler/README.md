# KWin Script for system76-scheduler Integration

Based on https://github.com/maxiberta/kwin-system76-scheduler-integration

This will notify System76 Scheduler via DBus when the active window changes, so that it can get
prioritized by the scheduler. The limitation is that `system76-scheduler` D-Bus service is published
on the system wide D-Bus, so it can only be used by the root user. This script can only communicate
with session D-Bus, so it

## Installing

Copy the `system76-scheduler` folder to `~/.local/share/kwin/scripts/`. It can't be symlinked, as
KWin won't detect it.

## Enabling

Once this is located at `~/.local/share/kwin/scripts/system76-scheduler-integration`, enable it by
running:

```sh
kwriteconfig5 --file kwinrc --group Plugins --key system76-scheduler-integrationEnabled true
qdbus org.kde.KWin /KWin reconfigure
```

Or just go to `System Settings > Window Management > KWin Scripts` and enable it from there.

## Developer docs

* https://develop.kde.org/docs/plasma/kwin
* https://develop.kde.org/docs/plasma/kwin/api

## Debugging

`print(QVariant...)`: prints the provided arguments to stdout. Takes an arbitrary number of
arguments. Comparable to console.log() which should be preferred in QML scripts.

The print output of Plasma and KWin scripts can be read from the journal:

```sh
journalctl -f QT_CATEGORY=js QT_CATEGORY=kwin_scripting
```
