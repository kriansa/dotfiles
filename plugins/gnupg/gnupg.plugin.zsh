# GnuPG agent activation
#
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"
# Export variables as recommended by 'man gpg-agent'
export GPG_TTY=$(tty)
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# These commands are meant to encrypt without metadata such as the key of the
# recipients. This way a message is "meaningless" if the recipient doesn't
# know to whom it was encrypted
alias gpg-anonymous-encrypt='gpg --encrypt --no-emit-version --no-comments --throw-keyids --armor'
alias gpg-anonymous-decrypt='gpg --decrypt --try-secret-key $GPG_MAIN_KEY'

# Sometimes GPG can be a PITA when you remove the card. When that happen, we
# just need to restart it
alias fix-gpg="killall -9 scdaemon gpg-agent; gpg -er "$GPG_MAIN_KEY" <<< \"It's working\" | gpg -d 2> /dev/null"
