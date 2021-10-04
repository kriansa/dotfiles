# Root plugin path
PLUGIN_PATH=$0:A:h

# 1. Exports
export PATH="$PLUGIN_PATH/bin:$PATH"

# 2. Useful aliases
alias d='docker'
alias dc='docker-compose'
alias k='kubectl'

# 3. Enable autocompletion commands
# Enable autocompletion for terraform
if [ $commands[terraform] ]; then
  autoload -U +X bashcompinit && bashcompinit
  complete -o nospace -C terraform terraform
fi
