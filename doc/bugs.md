# Bugs

This is a list of upstream bugs I'm currently working around and tracking for my setup.

## GNOME

* GNOME-Keyring password change sync
  Whenever we change passwords, the keyring doesn't seem to pick up the new password.

  The workaround is done [here](ansible/roles/gnome/tasks/configure_gnome.yml).

  See: https://bugs.archlinux.org/task/67846
