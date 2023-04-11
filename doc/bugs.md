# Bugs

This is a list of upstream bugs I'm currently working around and tracking for my setup.

## GNOME

* Wayland support
  In the past, my setup had a few issues with Wayland and Nvidia drivers, so I
  needed to disable it manually by explicitely setting it on GDM. Newer
  versions (> 3.30.1) has it disabled automatically for setups with Nvidia.

  The workaround for that was removed on 2cc16c102cee83a1c479efbf97f7e34472b299ac, however they
  added it back on GNOME 41.1. Unfortunately Wayland on proprietary NVIDIA is not at a state where
  all applications work seemlessly, so I had to disable it again (see it on
  `roles/gnome/tasks/configure_gnome.yml`). It still somewhat buggy on Alacritty and Firefox needs
  an extra env variable to support it, so I'm simply using X11 instead.

  It's important to note that between GDM releases 3.30.1 and 41.1, Wayland on NVIDIA had been
  disabled by default, apparently due to issues with GLX on proprietary NVIDIA drivers.

  More info: https://gitlab.gnome.org/GNOME/gdm/commit/5cd78602d3d4c8355869151875fc317e8bcd5f08
  See: https://gitlab.gnome.org/GNOME/gdm/blob/master/NEWS

* GNOME-Keyring password change sync
  Whenever we change passwords, the keyring doesn't seem to pick up the new password.

  The workaround is done [here](ansible/roles/gnome/tasks/configure_gnome.yml).

  See: https://bugs.archlinux.org/task/67846
