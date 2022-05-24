# Setup our helper aliases
abbr --global --add d docker
abbr --global --add dc docker-compose
abbr --global --add k kubectl
abbr --global --add kns kubens
abbr --global --add kcx kubectx

if type -q podman
  set --global --export DOCKER_HOST "unix://$XDG_RUNTIME_DIR/podman/podman.sock"
  set --global --export DOCKER_BUILDKIT 0
end
