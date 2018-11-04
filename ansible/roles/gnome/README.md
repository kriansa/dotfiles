# GNOME

This will install all GNOME-related applications in order to have a working
desktop environment.

## Issues

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
