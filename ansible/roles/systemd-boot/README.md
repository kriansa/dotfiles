# Systemd-boot

This is a role to configure systemd.

## Fixing Windows boot partition

This setup is made so that Windows EFI files are located at the same EFI partition we use for
everything else. The way we make it work is by manually forcing Windows EFI to locate the boot
partition.

In that case, it doesn't matter on which partition or disk Windows is currently installed. All we
need to do to make it dual boot is to run the install media and reinstall the EFI files on our EFI
partition. Here's the steps in order to do that:

1. Boot the computer using Windows install media
1. Select the keyboard language correctly, otherwise you won't be able to type special characters
   such as `/`
1. Select repair windows
1. Go to Troubleshoot -> Advanced -> Command Prompt
1. Type in the prompt
   ```cmd
   diskpart
   list volume
   ```
1. We want to figure which is the Windows partition and which one is the EFI. We want both to be
   assigned to a letter, so find the correct EFI partition in that list and then:
  ```cmd
  select volume <X>
  assign letter K:
  ```
  This will assign the EFI partition to the letter K. If your Windows partition (typically C:) is
  not assigned to any letter, repeat this step but selecting the correct partition. If you need,
  exit this prompt and run `dir C:/` and check if you see the `Windows` folder.
1. After ensuring you have both `C:` assigned to your Windows partition and `K:` to your EFI, then
   exit this prompt by typing `exit` and enter.
1. Now, run this command:
   ```cmd
   bcdboot C:\Windows /l pt-br /s K: /f UEFI /v
   ```
   That will install the bootloader in your EFI partition, under the path `EFI/Microsoft/Boot`.
1. That will also install an UEFI entry on your BIOS, so you will need to boot in BIOS and change
   the bootloader from Windows to Linux bootloader.
1. You should be good to go, but if you happen not to have the Linux bootloader installed, then
   boot up with Arch install media, mount the boot volume, chroot into /mnt and install
   `systemd-boot` by running `bootctl install`.

## In case diskpart is not recognizing the partitions

Remember to check if the partitions have the correct flags to it.
- **EFI Partition:** boot, esp, no_automont
- **Microsoft reserved partition:** msftres, no_automont
- **Windows partition:** msftdata, no_automont
- **Recovery partition:** hidden, diag, no_automont

## Links

[Arch Wiki - Systemd-boot](https://wiki.archlinux.org/index.php/systemd-boot)
