# Bugs

This is a list of current bugs I'm tracking for my setup.

## Motherboard ASUS Prime Z-370

### ACPI AE_NOT_FOUND

`dmesg` outputs the following ACPI Errors:

```
[    0.414729] ACPI BIOS Error (bug): Failure looking up [\_SB.PCI0.RP04.PXSX._SB.PCI0.RP05.PXSX], AE_NOT_FOUND (20180105/dswload2-194)
[    0.414733] ACPI Error: AE_NOT_FOUND, During name lookup/catalog (20180105/psobject-252)
[    0.414735] ACPI Error: Method parse/execution failed \_SB.PCI0.RP04.PXSX, AE_NOT_FOUND (20180105/psparse-550)
[    0.415068] ACPI BIOS Error (bug): Failure looking up [\_SB.PCI0.RP08.PXSX._SB.PCI0.RP09.PXSX], AE_NOT_FOUND (20180105/dswload2-194)
[    0.415070] ACPI Error: AE_NOT_FOUND, During name lookup/catalog (20180105/psobject-252)
[    0.415072] ACPI Error: Method parse/execution failed \_SB.PCI0.RP08.PXSX, AE_NOT_FOUND (20180105/psparse-550)
```

According to [this
answer](https://unix.stackexchange.com/questions/443398/acpi-bios-error-ae-not-found),
this might have to do with my 'too new' hardware.

* [Another related issue](https://superuser.com/questions/1277670/bios-acpi-error-message-when-installing-os-on-msi-z370-a-pro)

### Cause

It seems that the fix for [this other
issue](https://bugzilla.kernel.org/show_bug.cgi?id=199295) might fix mine as
well. These patch will be included in Kernel 4.17.

### Workaround

For now, we can safely ignore these errors.

* [Reddit thread](https://www.reddit.com/r/archlinux/comments/8iulc1/acpi_errors_on_xps_9370/)

### PCIe Bus Error

`dmesg` outputs the following errors:

```
[    1.080481] pcieport 0000:00:1c.4: PCIe Bus Error: severity=Corrected, type=Physical Layer, id=00e4(Receiver ID)
[    1.080482] pcieport 0000:00:1c.4:   device [8086:a294] error status/mask=00000001/00000000
[    1.080483] pcieport 0000:00:1c.4:    [ 0] Receiver Error         (First)
```

### Cause

It has to do with the PCI-E power saving feature. Somehow, the device has
turned a lower power state and may be causing these errors.

* [AskUbuntu thread](https://askubuntu.com/questions/863150/pcie-bus-error-severity-corrected-type-physical-layer-id-00e5receiver-id)

### Workaround

For now, we can safely ignore these errors.
