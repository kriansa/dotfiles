# Bugs

This is a list of upstream bugs I'm currently working around and tracking for my setup.

## nvidia

* Since 495.29+, NVIDIA drivers will flood `dbus` with messages and quickly fill up journal logs.

  While there are no current NVIDIA response on this subject, the community has come up with a
  workaround that involves creating a fake dbus service to blackhole all drivers messages and
  prevent cluttering your logs with garbage. Someone packaged that into a
  [AUR](https://aur.archlinux.org/packages/nvidia-fake-powerd/).

  This workaround is currently installed [here](ansible/roles/base-arch/tasks/install_nvidia.yml),
  and it uses a AUR that is managed by my Pacom install. Whenever this bug ceases to exist, we must
  also remove the `nvidia-fake-powerd` package.

  See: https://forums.developer.nvidia.com/t/bug-nvidia-v495-29-05-driver-spamming-dbus-enabled-applications-with-invalid-messages/192892/15

## systemd-resolved

* DNSSEC resolution is not falling back to normal DNS when a domain has a bad signature.

  This is being worked around [here](ansible/roles/base-arch/tasks/disable_resolved_dnssec.yml).

## PulseAudio

* Delayed volume changes
  PulseAudio enables by default "deferred volume", which is a way to overcome some issues caused by
  the "flat volume" configuration enabled by default on most distros.
  One of the side effects of deferred volume is a small delay while changing the volume of the HW
  output. On Arch, flat volume is disabled, so it doesn't make sense to keep deferred volume on as
  well.

  This is being worked around [here](ansible/roles/base-arch/tasks/fix_delayed_volume.yml).
 
  See: https://bugs.archlinux.org/task/46904

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

## Podman

Using podman as a complete replacement for `docker` is a work in progress. Although the developments
have evolved over the last year, it still has two major issues before I do the full transition:

1. Rootless containers aren't fully compatible with docker-compose yet due to a lack of possibility
   to set a default value for `userns`. This is reported and being tracked by
   `https://github.com/containers/podman/pull/11317` and `https://github.com/containers/podman/issues/11350`
2. Running spare containers off of a docker-compose with a running service is not currently
   possible. This is reported and being tracked by `https://github.com/containers/podman/issues/11717`

While both bugs aren't fully fixed, I'm running a workaround for the `userns` so that I always run
docker as root and therefore I won't be able to use the rooless mode. This is currently being done
by the files `plugins/devops/bin/docker` and `plugins/devops/bin/docker-compose`. When the first bug
is fixed then remove both of them, as well as the entry on `/etc/sudoers.d/10-wheel`

As for the second bug, the only thing is to avoid using `docker-compose run` for running services,
and replace that call by a `docker-compose exec` wherever possible.
