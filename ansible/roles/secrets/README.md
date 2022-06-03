# Secrets

This role is responsible for setting up all secret-based dotfiles that aren't shared withing this
git repo. It does so by first pulling secrets from the local NFS server then configuring all
Syncthing-based dotfiles.

## Configure Yubikey for U2F auth

Before starting to use Yubikey to auth yourself at a specific computer using U2F, you first need to
register that U2F key to that `origin` and that specific `username`. To do that, insert the Yubikey
andrun the following command:

```sh
pamu2fcfg --origin=pam://dpereira
```

The card will start flashing. Touch it and it will automatically write its contents to the stdout.
Now take it and write that to `~/.config/Yubico/u2f_keys`.

The origin parameter refers to the FIDO2 relying party ID that the host will be identified as in
order to proceed with the authentication.

See: https://developers.yubico.com/pam-u2f/#registration
