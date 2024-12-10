# Setup our helper aliases
abbr --global --add dc docker-compose
abbr --global --add k kubectl
abbr --global --add kns kubens
abbr --global --add kctx kubectx

# For kubectl
abbr_subcommand kubectl describe d
abbr_subcommand kubectl delete dl
abbr_subcommand kubectl get g

# Wrap kubectl with kubecolor
alias kubectl="kubecolor"
set --global --export KUBECOLOR_PRESET light
set --global --export KUBECOLOR_THEME_TABLE_COLUMNS white/blue
set --global --export KUBECOLOR_THEME_BASE_INFO white

# Binaries from kubectl krew
fish_add_path --path --global $HOME/.krew/bin
set --global --export KREW_NO_UPGRADE_CHECK 1

# Set default image for kubectl node-shell
set --global --export KUBECTL_NODE_SHELL_IMAGE nicolaka/netshoot

if type -q podman
  abbr --global --add d podman
  abbr --global --add p podman

  # On Podman Mac, `podman-mac-helper` will add a socket symlink to /var/run/docker.sock
  if ! test -S /var/run/docker.sock
    set --global --export DOCKER_HOST "unix://$XDG_RUNTIME_DIR/podman/podman.sock"
  end
else
  abbr --global --add d docker
end

set -x SSH_ASKPASS ssh-pass
set -x SSH_ASKPASS_REQUIRE prefer
