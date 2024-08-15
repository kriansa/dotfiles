# Install Arch base system
> Last update: Aug 15th, 2024.

First and foremost you will need to disable secure boot on your system. For newer ASUS motherboards,
[you will need to remove
PK](https://www.technorms.com/45538/disable-enable-secure-boot-asus-motherboard-uefi-bios-utility)
(platform key) to disable completely Secure Boot.

Secondly, this guide assumes that you've installed Windows using UEFI mode and that you left some
space left to install ArchLinux. If you haven't, some steps should be taken, such as creating a EFI
partition. See more info on [EFI System Partition
wiki](https://wiki.archlinux.org/index.php/EFI_System_Partition).

## Part 1: Downloading ISO and flashing the USB drive

1. Download the image and their signature available at https://www.archlinux.org/download/
2. Ensure that you've checked the signature by:
   > $ pacman-key -v archlinux-*-x86_64.iso
3. Flash the image on the drive [using
   dd](https://wiki.archlinux.org/index.php/USB_flash_installation_media).

## Part 2: Configuring and installing arch

You will need to boot from the USB drive. Ensure that you choose UEFI boot, whenever possible. Once
that you see a prompt on the screen, you can proceed with the commands below. All the steps
documented here were extracted from [Arch installation
guide](https://wiki.archlinux.org/index.php/Installation_guide) and many other linked wikis. It's
advisable to read it if you have questions.

##### 1. (optional) Load the keyboard setup if you use a keyboard standard different than US.
    # loadkeys br-abnt

##### 2A. Connect to the network (WLAN)
    # iwlist scan
    # iwctl --passphrase <password> station wlan0 connect[-hidden] <SSID>

##### 2B. Connect to the network (Ethernet)

If DHCP is enabled, you should simply check if it's already connected.

    # ip a

##### 3. Update system clock
    # timedatectl set-ntp true

##### 4. Now you should partition your disk. To list all available disk and partitions, type:
    # fdisk -l

**Important**: The steps below are for when your setup is using EFI instead BIOS. To check if you're
using EFI, run `efivar --list`

Once you know which disk you will partition/install, lets partition it using GNU parted.

##### 4. Partition the disk
    # parted /dev/sdX

Replace `sdX` by the disk that you want to install Arch on.

##### 1. To list all disk and current partitions, you run:
    (parted) print all

If you see an error like: /dev/sdX: unrecognized disk label That means that your disk partition
table hasn't been initialized on either GPT (UEFI) or MBR (msdos)
To initialize using GPT, run the command (on parted):
`(parted) mklabel gpt`

**Important:** You will need at least 2 partitions:
* One partition `fat32` for EFI boot and bootloader - Bootable (1024MB). Assuming that Windows is
  already installed, this partition already exists and it's 100MB length -- you will need to extend
  it.
* One partition `xfs` to be used as a base for your LVM (All space left) This one can be created
  with the command below.
* If your disk already have a `lvm` set and you want to remove it, you can use `lvs` to list all
  available lvs and then `lvchange -an <vgname>` to unmount them.

###### 2A. _(optional)_ Extend existing EFI partition

Because EFI is located at the begining of the disk, you will need to move all existing partitions
before continuing. You need to extend it by at least `400MB`, so keep that number in mind because
all partitions after EFI will need to be shifted by that amount.

Doing that procedure over the CLI is difficult and too error prone. A simpler and efficient way of
doing that is booting an Ubuntu and moving all partitions using GParted. So far it has proven safe
even for BitLocker-encrypted partitions.

###### 2B. _(optional)_ Create the EFI partition

> That is usually **NOT** required because you will have one already created by Windows installer.

    (parted) mkpart primary 1024KiB 1GB
    (parted) set 1 esp on
    (parted) name 1 "EFI system partition"

Replace 1 by the number of the EFI partition (use `print` to list them).

##### 3. Create a new partition to use the entire disk
    (parted) mkpart primary 2MB 100%
    (parted) print
    (parted) name X "Linux LVM"

This command may ask if you want to change the beginning of the sector (0).
Answer `yes`. Then use `print` to list partition IDs and then rename it.

##### 4. Now, quit parted
    (parted) quit

##### 5. (optional) Enable LUKS (Disk Encryption)
    # cryptsetup luksFormat /dev/sdXY
    # cryptsetup --allow-discards --persistent open /dev/sdXY luks-lvm

> **Important:** Replace `sdXY` by that big partition that you've created for Linux.

##### 6A. Setup partitioning using LVM and LUKS
    # pvcreate /dev/mapper/luks-lvm
    # vgcreate vg0 /dev/mapper/luks-lvm

##### 6B. Setup partitioning using LVM WITHOUT LUKS:
    # pvcreate /dev/sdX5
    # vgcreate vg0 /dev/sdX5

> **Important:** Replace `sdX5` by that big partition that you've created for Linux.

##### 7. Creates several Logical Groups (LV)
    # lvcreate --size 32G vg0 --name swap
    # lvcreate --size 80G vg0 --name root
    # lvcreate --extents +100%FREE vg0 --name home

> **Important:** Adjust the swap to the size of your RAM so you can use hibernation.

##### 8. Create filesystems on those partitions
    # mkswap /dev/vg0/swap
    # mkfs.xfs /dev/vg0/root
    # mkfs.xfs /dev/vg0/home

##### 8.1. (optional) If needed, format the EFI partition
    # mkfs.fat -F32 /dev/sdXY

##### 9. Mount the new system
    # mount /dev/vg0/root /mnt
    # swapon /dev/vg0/swap
    # mkdir /mnt/home
    # mount /dev/vg0/home /mnt/home
    # mkdir /mnt/boot
    # mount /dev/sdXY /mnt/boot

> EFI Bootloader is in the separate partition (marked by `sdXY`).
> Replace it by the partition that you found when you ran `fdisk -l`

> If your system uses cable connection and dhcp, it's already configured.

##### 10. Install Arch base packages
    # pacstrap -K /mnt base lvm2 xfsprogs linux linux-firmware sudo <intel|amd>-ucode networkmanager neovim

##### 11. Generate the fstab file so every disk is mapped and loaded at boot time
    # genfstab -U /mnt >> /mnt/etc/fstab

##### 12. Enter in the new system
    # arch-chroot /mnt

> **Notice:** Now, you're in the filesystem of your new installation. Any
> change that you do here will be persisted.

##### 13. Set the default keymap (use `br-abnt` for the Portuguese version)
    # echo "KEYMAP=us" > /etc/vconsole.conf

##### 14. Configure initcpio
Edit `/etc/mkinitcpio.conf` this way:
1. Replace `udev` by `systemd` on HOOKS
1. Add `xfs` in the MODULES line
1. Set `yes` to the MODULES_DECOMPRESS line

##### 14A. Configure boot (with LUKS)
Edit `/etc/mkinitcpio.conf` this way:
1. Add `keyboard` before `autodetect` on HOOKS
1. Add `sd-vconsole sd-encrypt lvm2` between `block` and `filesystems` on HOOKS

##### 14B. Configure boot (without LUKS)
If you aren't using LUKS, then configure `/etc/mkinitcpio.conf` this way:
1. Add `lvm2` between `block` and `filesystems` on HOOKS

##### 15. Generate initramfs
    # mkinitcpio -P

##### 16. Set a root password
    # passwd

##### 17. Install [systemd-boot](https://wiki.archlinux.org/index.php/Systemd-boot)
    # bootctl install

##### 18. Configure Linux bootloader entry
    # nvim /boot/loader/entries/arch.conf

Add the content below to this file:

```
title   Arch Linux
linux   /vmlinuz-linux
initrd  /<intel|amd>-ucode.img
initrd  /initramfs-linux.img
options root=/dev/vg0/root resume=/dev/vg0/swap rw rootflags=x-systemd.device-timeout=30
```

> Replace the `<intel|amd>` properly

##### 18.1. (optional) Add options for booting with LUKS
When using LUKS, replace the last line of the file above by:

```
options        root=/dev/vg0/root resume=/dev/vg0/swap rw rootflags=x-systemd.device-timeout=30 rd.luks.options=discard,timeout=30 rd.luks.name=$(blkid -s UUID -o value /dev/sdXY)=vg0
```

##### 19. Exit new system and go into the cd shell
    # exit

##### 20. Unmount all partitions
    # umount -R /mnt
    # swapoff -a

##### 21. Reboot the system
    # reboot

## Configuring Arch

After rebooting, you will have a very basic form of Arch installed, but you will need to configure
it and install your applications. This step can be done automatically for you with Ansible.

But first, let's ensure that we have internet connection, so start the network manager:

    # systemctl start NetworkManager

If you need wi-fi, also run:

    # nmcli dev wifi connect  <SSID> name Wi-Fi password <PASSWORD> [hidden yes]

From here you will have two options: either running Ansible from the system locally or remotely.
Pick one and follow the steps.

#### A) Local configuration (recommended)

##### 1. Install required packages
    # pacman -S git ansible

##### 2. Clone the repo into /tmp
    # cd /tmp && git clone --depth 1 https://github.com/kriansa/dotfiles && cd dotfiles

##### 3. Run it
    # bin/setup localhost <laptop|desktop>

##### 4. Run the additional steps asked on the screen, then reboot
    # reboot

#### B) Remote configuration

##### 1. Locally, install and start SSH
    # pacman -S openssh
    # systemctl start sshd
    # install -m 0600 <(curl https://github.com/kriansa.keys) .ssh/authorized_keys
    # ip a

##### 2. On the remote computer, run:
    $ bin/setup root@<ip> <laptop|desktop> [--debug]

##### 3. Reboot
    $ ssh <ip> reboot
