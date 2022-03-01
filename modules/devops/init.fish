# Setup our helper aliases
abbr --global --add d docker
abbr --global --add dc docker-compose
abbr --global --add k kubectl

if type -q podman
  set --global --export DOCKER_HOST "unix://$XDG_RUNTIME_DIR/podman/podman.sock"
end
