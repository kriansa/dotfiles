# Bugs

This is a list of upstream bugs I'm currently working around and tracking for my setup.

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

  The workaround for that was removed on 2cc16c102cee83a1c479efbf97f7e34472b299ac.

  If you ever face something weird again, just reactivate the task with the 
  following lines:

  ```yaml
  # If GDM ever gets troublesome when using Wayland, it's better to disable it
  # from loading from Wayland and use Xorg instead
  # This is an issue with NVIDIA
  # See: https://bugs.archlinux.org/task/53284
    - name: disable Wayland on GDM
    become: true
    when: "'nvidia' in role_names"
    lineinfile:
  path: /etc/gdm/custom.conf
  line: "WaylandEnable=false"
  regexp: "#?WaylandEnable=false"
  ```

  It's important to note that since GDM release 3.30.1, Wayland on NVIDIA has been disabled by
  default, apparently due to issues with GLX on proprietary NVIDIA drivers.

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
