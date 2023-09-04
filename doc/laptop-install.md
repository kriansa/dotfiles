# Arch Linux Laptop Installation

## Installing the base system

Follow [this page](./arch-install.md) to install the base system.

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
    # bin/setup localhost laptop

##### 4. Run the additional steps asked on the screen, then reboot
    # reboot

#### B) Remote configuration

##### 1. Locally, install and start SSH
    # pacman -S openssh python
    # systemctl start sshd
    # mkdir -m 700 .ssh
    # install -m 600 <(curl https://github.com/kriansa.keys) .ssh/authorized_keys

##### 2. On the remote computer, run:
    $ bin/setup root@<ip> laptop [--debug]

##### 3. Reboot
    $ ssh <ip> reboot
