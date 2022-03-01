# Export variables as recommended by 'man gpg-agent'
set --global --export GPG_TTY (tty)
set --erase SSH_AGENT_PID

if not set -q gnupg_SSH_AUTH_SOCK_by or test $gnupg_SSH_AUTH_SOCK_by -ne %self
  set --global --export SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
end

# Set my default GPG Key
set --global --export GPG_MAIN_KEY "BC277BE09E8A6F1059C3911B3E7884756312F945"
