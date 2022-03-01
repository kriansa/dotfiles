# GnuPG

## Tips

Anonymously encrypt/decrypt:

```bash
gpg --encrypt --no-emit-version --no-comments --throw-keyids --armor > encrypted.asc
gpg --decrypt --try-secret-key $GPG_MAIN_KEY encryted.asc > decrypted.asc
```
