# Windows setup

## Install
1. Boot and install OS (pick Windows 11 Enterprise LTSC IoT)
1. On the user activation screen, hit Shift + F10
1. Type in `start ms-cxh:localonly` and create a new user. Set a password, otherwise you won't be
   able to log in through RDP or setup auto-login.

## After installation

### 1A) For the Desktop
1. Install i226v Drivers
1. Install Firefox, PowerToys, Sysinternals
1. Disable Linux disk:
   1. Press Windows + X and select the Disk Manager option;
   1. After loading all connected disks, right click on the NVME you want to disable and go to Properties;
   1. Click on the Driver tab;
   1. Click the Disable Device button.

### 1B) For a server
1. Enable RDP (Remote Access)
1. Install Windows VirtIO drivers. See: https://pve.proxmox.com/wiki/Windows_VirtIO_Drivers

### 2) For all of them
1. Setup updates
1. Disable search index by running on Admin CMD: `sc stop "wsearch" && sc config "wsearch" start=disabled`
1. Enable automatic login. See: https://learn.microsoft.com/en-us/troubleshoot/windows-server/user-profiles-and-logon/turn-on-automatic-logon
1. Remove "About this picture". See: https://www.guidingtech.com/how-to-remove-learn-about-this-picture-from-windows-11/
1. After getting to the desktop, activate it by opening Powershell and typing: `irm https://get.activated.win | iex` then use `HWID` if possible.
   See: https://massgrave.dev/

# Troubleshooting

* [Fixing boot partition](../ansible/roles/systemd-boot/README.md)
