# Setup our helper aliases
abbr --global --add dc docker-compose
abbr --global --add k kubectl
abbr --global --add kns kubens
abbr --global --add kctx kubectx

# For kubectl
abbr_subcommand kubectl describe d
abbr_subcommand kubectl get g

if type -q podman
  abbr --global --add d podman
  abbr --global --add p podman
  set --global --export DOCKER_HOST "unix://$XDG_RUNTIME_DIR/podman/podman.sock"
  set --global --export DOCKER_BUILDKIT 0
else
  abbr --global --add d docker
end

set -x SSH_ASKPASS ssh-pass 
set -x SSH_ASKPASS_REQUIRE prefer
