# Bugs

This is a list of current bugs I'm tracking for my setup.

## systemd-resolved

DNSSEC resolution is not falling back to normal DNS when a domain has a bad signature.

This is being worked around [here](ansible/roles/base-arch/tasks/disable_resolved_dnssec.yml).
