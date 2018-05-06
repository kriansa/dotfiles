# Grub

Grub is a bootloader, a tool used to fire up the OS's on the machine. 

Grub is mostly used for when we have a MBR BIOS boot, very common on older
systems. Although Grub is compatible with UEFI systems, it's recommended
that you use [systemd-boot](https://wiki.archlinux.org/index.php/systemd-boot).

* [Boot Loaders](https://wiki.archlinux.org/index.php/Category:Boot_loaders)

For this setup, I'll be using it only for when I'm on my MBR BIOS based
computer. For my UEFI machines, I'll be using systemd-boot.

## More details

On my Desktop machine setup, I have installed Windows on my brand-new
hard-drive. At the end of the process, Windows has created two partitions:
* A boot partition, and;
* A system partition.

The way that I dual-boot it is by installing Linux in a third and fourth
partitions, and making the third one the `/boot` partition, with **Grub** in
it.

Therefore, in the end of the installation process, I'll end up having the
following schema:

1. ntfs Partition - Windows Loader
2. ntfs Partition - OS Files
3. ext4 Partition - Grub - **bootable flag**
4. ext4 Partition - Linux - LVM Volume

Remember to change the variables located at `defaults/main.yml` if you need to
use a different setup.

## Dependencies

This `role` depends on `base-arch`, particularly on a handler (rebuild grub).
