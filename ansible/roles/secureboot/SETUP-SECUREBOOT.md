# Secure Boot setup (dual boot Arch + Windows)

Machine: single ESP at `/boot`, shared between Arch (systemd-boot) and Windows.
Root filesystem (`vg1-root`) is unencrypted LVM, so signing keys are TPM-shielded
where possible (see step 4).

Approach: **custom keys via `sbctl`, with Microsoft's certificates enrolled
alongside them**, so Windows Boot Manager stays trusted under Secure Boot.
(Alternative — shim + MOK, keeping only Microsoft's factory keys — was ruled
out for systemd-boot: it needs a `grubx64.efi`-renaming hack and hand-rolled
pacman hooks to re-sign the bootloader/kernel on every update, with no
maintained tooling. `sbctl` ships its own pacman hook that does this
automatically.)

## Steps

1. Install `sbctl`:
   ```
   sudo pacman -S sbctl
   ```

2. Reboot into firmware setup and enter Secure Boot **Setup Mode**:
   ```
   systemctl reboot --firmware-setup
   ```
   In the Secure Boot menu, clear/delete the existing keys (at minimum the
   Platform Key). Use the option that actually enters **Setup Mode**, not
   "Custom Mode" (Custom Mode only disables signature checks, it doesn't let
   you enroll new keys). Leave Secure Boot toggled on if the firmware allows
   that with no keys present.

3. Confirm Setup Mode is active:
   ```
   sbctl status
   ```

4. Generate keys, with PK/KEK shielded in the TPM (db key stays a plain file —
   sbctl's own recommended split when root isn't encrypted):
   ```
   sudo sbctl create-keys --pk-keytype tpm --kek-keytype tpm
   ```

5. Enroll the keys plus Microsoft's certificates, so Windows Boot Manager
   keeps working:
   ```
   sudo sbctl enroll-keys --microsoft
   ```
   If this errors about an "Option ROM" (unsigned firmware on a device such as
   a discrete GPU), read `sbctl`'s Option ROM FAQ before considering the
   `--yolo` override — don't force it blindly.

6. Sign the boot chain. Leave Windows's own binary alone — it's already
   Microsoft-signed and covered by the `--microsoft` enrollment above.
   ```
   sudo sbctl sign --save /boot/EFI/systemd/systemd-bootx64.efi
   sudo sbctl sign --save /boot/EFI/systemd/systemd-boot-fallbackx64.efi
   sudo sbctl sign --save /boot/EFI/BOOT/BOOTX64.EFI
   sudo sbctl sign --save /boot/vmlinuz-linux
   sudo sbctl sign --save /boot/vmlinuz-linux-lts
   ```

7. Verify everything signed above shows as signed:
   ```
   sudo sbctl verify
   ```

8. Reboot and confirm:
   ```
   systemctl reboot
   sbctl status
   ```
   Expect `Setup Mode: disabled`, `Secure Boot: enabled`. If Secure Boot shows
   off, go back into firmware once more and flip it on — some boards reset the
   toggle when their keys are cleared.

9. Boot into Windows once and confirm via `msinfo32` that "Secure Boot State"
   says On.

## Maintenance

The `sbctl` Arch package ships a pacman hook (`zz-sbctl.hook`) that runs
`sbctl sign-all -g` automatically after any transaction touching `/boot`,
`/efi`, or kernel modules, plus a `kernel-install` plugin that signs new
kernel entries as they appear. Future `linux` / `linux-lts` / `systemd`
upgrades re-sign themselves — no hooks to maintain by hand.

Only exception: a **new** kernel package (beyond `linux` and `linux-lts`,
already covered above) needs one manual
`sbctl sign --save /boot/vmlinuz-<name>` the first time, so it's tracked.
After that it's automatic.
