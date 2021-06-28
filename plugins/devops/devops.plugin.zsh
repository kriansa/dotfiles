# Root plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# 2. Useful aliases
alias d='docker'
alias dc='docker-compose'
alias k='kubectl'

k-list-all() {
	namespace=$1

	kubectl api-resources --verbs=list --namespaced -o name | \
		xargs -n 1 kubectl get --show-kind --ignore-not-found -n $namespace
}

# This helps calling a URL by resolving its address to any arbitrary IP
debug-tls-cert() {
  ip=$1
  host=$2

  curl -kvv --no-keepalive -H "Host: $host" --resolve "$host:443:$ip" "https://$host"
}

# 3. Enable autocompletion commands
# Enable autocompletion for terraform
if [ $commands[terraform] ]; then
  autoload -U +X bashcompinit && bashcompinit
  complete -o nospace -C terraform terraform
fi
