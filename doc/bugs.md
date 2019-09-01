# Bugs

This is a list of current bugs I'm tracking for my setup.

## systemd-resolved

DNSSEC resolution is not falling back to normal DNS when a domain has a bad signature.

This is being worked around [here](ansible/roles/base-arch/tasks/disable_resolved_dnssec.yml).

## nvidia

#### Date: June 11, 2019

NVIDIA drivers are problematic and not working with my computer since 430.14. The workaround was to
have a pinned version of it on my own [AUR
repository](https://github.com/kriansa/PKGBUILDs/tree/master/pkgs/nvidia-418).

When that gets fixed, I will need to remove the extra package from
`ansible/roles/nvidia/files/nvidia.hook` as well as from `/etc/pacman.d/hooks/nvidia.hook`.

## rapid-photo-downloader

#### Date: Sept 1, 2019

RPD seems to be with a missing python dependency from default install and we need to install it
separately. The dependency is `python-tenacity`.

A bug report is [open](https://bugs.archlinux.org/task/63468) and the workaround is to install it
along with the package.
