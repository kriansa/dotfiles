# Setup our helper aliases
abbr --global --add k kubectl
abbr --global --add kns kubens
abbr --global --add kctx kubectx

# For kubectl
abbr_subcommand kubectl describe d
abbr_subcommand kubectl delete dl
abbr_subcommand kubectl get g
abbr_subcommand kubectl "config current-context" st

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

# Disable "hints" for Docker CLI
set --global --export DOCKER_CLI_HINTS 0

if type -q podman
  abbr --global --add docker podman
  abbr --global --add d podman
  abbr --global --add p podman
  abbr --global --add dc podman compose
  abbr --global --add docker-compose podman compose

  # BuildKit is no compatible with using PODMAN_USERNS=keep-id
  # Disabling it is the compromise we make to ensure docker-compose is compatible with both docker
  # and podman.
  set --global --export DOCKER_BUILDKIT 0

  # Disable warning logs when using `podman compose`
  set --global --export PODMAN_COMPOSE_WARNING_LOGS 0

  # When running the binary docker-compose directly, you will need DOCKER_HOST to be set beforehand.
  # However, when it's run through `podman compose`, then it will be directly assigned and you won't
  # have to worry. In case you're running this binary directly, you have two options:
  #   1. Set it manually. Useful when the path is static (like on Linux)
  #   2. On Mac, run `sudo podman-mac-helper install` and it will create a symlink at
  #   `/var/run/docker.sock` pointing to the dynamic one automatically everytime `podman machine
  #   start` is run.
  #
  # The condition below would set this variable when needed.
  if ! test -S /var/run/docker.sock
    set --global --export DOCKER_HOST "unix://$XDG_RUNTIME_DIR/podman/podman.sock"
  end
else
  abbr --global --add d docker
  abbr --global --add dc docker-compose
end

set -x SSH_ASKPASS ssh-pass
set -x SSH_ASKPASS_REQUIRE prefer

# Tell K9s to use the same dir both on Linux as well as on macOS
set -x K9S_CONFIG_DIR $HOME/.config/k9s
