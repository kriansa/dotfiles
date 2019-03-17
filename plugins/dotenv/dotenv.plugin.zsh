# Loads the variables definied on the .env file onto the shell
# If you wish, you can pass a parameter as the name of the file instead of using the default (.env)

function dotenv {
  local file=${1:-.env}

  if [ ! -f "$file" ]; then
    echo "File '$file' not found!" >&2
    return 1
  fi

  set -a
  source "$file"
  set +a
}
