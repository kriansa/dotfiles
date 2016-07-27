# GnuPG agent activation
#
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh

# 2. Alias gpg to gpg2
alias gpg='gpg2'
# This commands are meant to encrypt without metadata such as the key of the recipients.
# This way a message is "meaningless" if the recipient doesn't know to whom it was encrypted
alias gpg-anonymous-encrypt='gpg2 --encrypt --no-emit-version --no-comments --throw-keyids --armor'
alias gpg-anonymous-decrypt='gpg2 --decrypt --default-recipient-self'
