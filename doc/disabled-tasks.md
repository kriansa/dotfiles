# Disabled Ansible tasks

This is where I keep track of the playbooks or tasks that I disabled but someday might want to add
back.

## VPN

This is my client VPN setup I have for connecting to my own deployed OpenVPN server. Because I'm not
actively using it, I found that it's best just to disable it for now.

I also created some very useful tunnel helpers to make an application to connect to the internet
using that interface. Those are not disabled and are available on the `unix` plugin.

* iftun
* tunssh
* tunscp

### To re-enable it:

Add the entry back to `ansible/roles/desktop-tools/tasks/main.yml`:

```yaml
- import_tasks: setup_vpn.yml
```

Add the necessary package installation to `ansible/roles/dev-tools/tasks/install_packages.yml`:

```yaml
# Install OpenVPN NetworkManager plugin
- networkmanager-openvpn
```
