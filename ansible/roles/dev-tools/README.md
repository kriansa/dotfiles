# Development Environment

This is usually stuff that I setup for a development machine.

## Compatibility between rootful Podman docker-compose

Currently I'm using user-wide rootless podman service to empower docker-compose. However, in the
rarest cases where it doesn't get to behave exactly as Docker, we could re-enable the rootful
version and set the `DOCKER_HOST` properly. To ensure max compatibility we can add the following
snippet as well, so that shared volumes behave identically as in Docker.

```yaml
- name: create podman.service.d systemd drop-in folder
  become: true
  ansible.builtin.file: path=/etc/systemd/system/podman.service.d state=directory

- name: set the default userns keep-id so that rootful docker-compose is identical to docker
  become: true
  register: set_default_userns
  ansible.builtin.copy:
    dest: /etc/systemd/system/podman.service.d/set-default-userns.conf
    mode: "0644"
    content: |
      # This file will be appended to the existing configuration of podman.service (systemd calls

      # a drop-in) and it will add this setting below for podman server. It is necessary so that

      # get the same behavior on rootless Podman with docker-compose as we do with Docker: files

      # the container have the same ownership metadata as they do in the host.
      [Service]
      Environment=PODMAN_USERNS=keep-id

- name: reload systemd
  become: true
  when: set_default_userns.changed
  ansible.builtin.systemd: daemon_reload=yes

- name: enable podman.socket so we can use docker-compose
  become: true
  ansible.builtin.systemd: state=restarted enabled=yes name=podman.socket

```
