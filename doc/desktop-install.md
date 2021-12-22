# Install Arch on Desktop
> Last update: December 22st, 2021.

First and foremost you will need to disable secure boot on your system. For newer ASUS motherboards,
[you will need to remove
PK](https://www.technorms.com/45538/disable-enable-secure-boot-asus-motherboard-uefi-bios-utility)
(platform key) to disable completely Secure Boot.

Secondly, this guide assumes that you've installed Windows 10 using UEFI mode and that you left some
space left to install ArchLinux. If you haven't, some steps should be taken, such as creating a EFI
partition. See more info on [EFI System Partition
wiki](https://wiki.archlinux.org/index.php/EFI_System_Partition).

## Part 1: Downloading ISO and flashing the USB drive

1. Download the image available at https://www.archlinux.org/download/
2. Ensure that you've checked the signature by:
   > $ shasum archlinux-2018.04.01-x86_64.iso
     42cf488fb6cba31c57f8ad875cb03784760c4b94  archlinux-2018.04.01-x86_64.iso
3. Flash the image on the drive [using
   dd](https://wiki.archlinux.org/index.php/USB_flash_installation_media).

## Part 2: Configuring and installing arch

You will need to boot from the USB drive. Ensure that you choose UEFI boot, whenever possible. Once
that you see a prompt on the screen, you can proceed with the commands below. All the steps
documented here were extracted from [Arch installation
guide](https://wiki.archlinux.org/index.php/Installation_guide) and many other linked wikis. It's
advisable to read it if you have questions.

##### 1. (Optional) Load the keyboard setup if you use a keyboard standard different than US.
    # loadkeys br-abnt

##### 2. Update system clock
    # timedatectl set-ntp true

##### 3. Now you should partition your disk. To list all available disk and partitions, type:
    # fdisk -l

**Important**: The steps below are for when your setup is using EFI instead BIOS. To check if you're
using EFI, run `efivar --list`

Once you know which disk you will partition/install, lets partition it using GNU parted.

##### 4. Partition the disk
    # parted /dev/sdX
    
Replace `sdX` by the disk that you want to install Arch on.

##### To list all disk and current partitions, you run:
    (parted) print all

> If you see an error like: /dev/sdX: unrecognized disk label That means that your disk partition
> table hasn't been initialized on either GPT (UEFI) or MBR (msdos)
> To initialize using GPT, run the command (on parted):
> `(parted) mklabel gpt`

> **Important:** You will need at least 2 partitions:
> * One partition `fat32` for EFI boot and bootloader - Bootable (512MB) Assuming that Windows is
>   already installed, this partition already exists and it's 100MB length.
> * One partition `ext4` to be used as a base for your LVM (All space left) This one can be created
>   with the command below.

##### (If needed) Create the EFI partition

    (parted) mkpart primary fat32 0% 512MB
    (parted) set 1 esp on

Replace 1 by the number of the EFI partition (use `print` to list them).

##### Create a new partition to use the entire disk
    (parted) mkpart primary ext4 512MB 100%

This command may ask if you want to change the beginning of the sector (0).
Answer `yes`.

##### Now, quit parted
    (parted) quit

#### 5A. Enable LUKS (Disk Encryption). Jump to the next step if you don't want it
---

##### 1. Setup the encryption of the system
    # cryptsetup -c aes-xts-plain64 -y --use-random luksFormat /dev/sda2
    # cryptsetup luksOpen /dev/sda2 luks-lvm

##### 2. Setup partitioning using LVM (creates the PV and VG)
    # pvcreate /dev/mapper/luks-lvm
    # vgcreate vg0 /dev/mapper/luks-lvm
---

##### 5B. If you're not going to use LUKS, create normal LVM:
    # pvcreate /dev/sdX5
    # vgcreate vg0 /dev/sdX5

> **Important:** Replace `sdX5` by that big partition that you've created for Linux.

##### 6. Creates several Logical Groups (LV)
    # lvcreate --size 16G vg0 --name swap
    # lvcreate --size 20G vg0 --name root
    # lvcreate --extents +100%FREE vg0 --name home

> **Important:** Adjust the swap to the size of your RAM so you can use hibernation.

##### 7. Create filesystems on those partitions
    # mkswap /dev/vg0/swap
    # mkfs.ext4 /dev/vg0/root
    # mkfs.ext4 /dev/vg0/home

##### 7.5. If needed, format the EFI partition
    # mkfs.fat -F32 /dev/sdZZ

##### 8. Mount the new system
    # mount /dev/vg0/root /mnt
    # swapon /dev/vg0/swap
    # mkdir /mnt/home
    # mount /dev/vg0/home /mnt/home
    # mkdir /mnt/boot
    # mount /dev/sda2 /mnt/boot 

> EFI Bootloader is in the separate partition (by default it's on `sda2`).
> Replace it by the partition that you found when you ran `fdisk -l`

##### (Optional) If your system has wi-fi, configure your connection
    # wifi-menu

> If your system uses cable connection and dhcp, it's already configured. 

##### 9. Install Arch base packages
    # pacstrap /mnt base lvm2 linux linux-firmware sudo intel-ucode networkmanager neovim

> If you need wi-fi, add packages: `iw` and `wpa_supplicant` to this list
> If you don't have an Intel CPU, remove `intel-ucode` from this list

##### 10. Generate the fstab file so every disk is mapped and loaded at boot time
    # genfstab -U /mnt >> /mnt/etc/fstab

##### 11. Enter in the new system
    # arch-chroot /mnt
    
> **Notice:** Now, you're in the filesystem of your new installation. Any
> change that you do here will be persisted.

##### 12. (Optional) Set the default keymap
    # echo "KEYMAP=br-abnt" > /etc/vconsole.conf

##### 13A. Configure boot (with LUKS)

First edit `/etc/mkinitcpio.conf` in the HOOKS line, and:
1. Add `encrypt lvm2 resume` between `block` and `filesystems` on HOOKS
2. Add `keyboard keymap` before `block` on HOOKS
3. Add `ext4` in the MODULES line

##### 13B. Configure boot (without LUKS)

If you aren't using LUKS, then configure `/etc/mkinitcpio.conf` this way:
1. Replace `udev` by `systemd` on HOOKS
2. Add `lvm2` between `block` and `filesystems` on HOOKS

> **Optional:** Regardless of LUKS, if you have enough space on your EFI partition, let's also
> disable the compression by adding the following line:
> COMPRESSION="cat"

##### 14. Generate initramfs
    # mkinitcpio -P

##### 15. Set a root password
    # passwd

##### 16. Install [systemd-boot](https://wiki.archlinux.org/index.php/Systemd-boot)
    # bootctl install

##### 17. Configure Linux bootloader entry
    # nvim /boot/loader/entries/arch.conf

Add the content below to this file:

    title          Arch Linux
    linux          /vmlinuz-linux
    initrd         /intel-ucode.img
    initrd         /initramfs-linux.img
    options        root=/dev/vg0/root resume=/dev/vg0/swap rw cryptdevice=PARTUUID=$(blkid -s PARTUUID -o value /dev/sda2):vg0

> If you're not using Intel, you can remove intel-ucode from the list below
> If you're not using LUKS, remove the content after cryptdevice

##### 18. Exit new system and go into the cd shell
    # exit

##### 19. Unmount all partitions
    # umount -R /mnt
    # swapoff -a

##### 20. Reboot the system
    # reboot

## Part 3: Configuring Arch

After rebooting, you will have a very basic form of Arch installed, but you will need to configure
it and install your applications. This step can be done automatically for you with Ansible.

But first, let's ensure that we have internet connection, so start the network manager:

    # systemctl start NetworkManager

If you need wi-fi, also run:

    # nmcli dev wifi connect <SSID> password <PASSWORD>

From here you will have two options: either running Ansible from the system locally or remotely.
Pick one and follow the steps.

#### A) Local configuration (recommended)

##### 1. Install required packages
    # pacman -S git ansible

##### 2. Clone the repo into /tmp
    # cd /tmp && git clone --depth 1 https://github.com/kriansa/dotfiles && cd dotfiles

##### 3. Run it
    # bin/setup localhost desktop

##### 4. Run the additional steps asked on the screen, then reboot
    # reboot

#### B) Remote configuration

##### 1. Locally, install and start SSH
    # pacman -S openssh
    # systemctl start sshd

##### 2. On my remote computer, run:
    $ bin/remote-setup root@<ip> desktop

##### 3. Reboot
    $ ssh <ip> reboot
