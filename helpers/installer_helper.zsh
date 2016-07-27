#!/usr/bin/env sh
#
# Small library to help with installation scripts

set -e

function info {
  printf "  [ \033[00;34m..\033[0m ] $1"
}

function user {
  printf "\r  [ \033[0;33m?\033[0m ] $1 "
}

function success {
  printf "\r\033[2K  [ \033[00;32mOK\033[0m ] $1\n"
}

function fail {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] $1\n"
  echo ''
  exit 1
}

function link_file {
  local plugin_path=$1
  local src="$plugin_path/include/$2" dst="$HOME/$2"
  local action=

  local overwrite= backup= skip=
  local action=

  if [ -f "$dst" -o -d "$dst" -o -L "$dst" ]; then

    local currentSrc="$(readlink $dst)"

    if [[ "$currentSrc" == "$src" ]]; then
      skip=true
    else

      user "File already exists: $dst ($(basename "$src")), what do you want to do?\n\
      [s]kip, [o]verwrite, [b]ackup?"
      read -k 1 action

      case "$action" in
        o ) overwrite=true ;;
        b ) backup=true ;;
        s ) skip=true ;;
      esac
    fi

    if [[ "$overwrite" == "true" ]]; then
      rm -rf "$dst"
      success "removed $dst"
    fi

    if [[ "$backup" == "true" ]]; then
      mv "$dst" "${dst}.backup"
      success "moved $dst to ${dst}.backup"
    fi

    if [[ "$skip" == "true" ]]; then
      success "skipped $src"
    fi
  fi

  if [[ "$skip" != "true" ]]; then  # "false" or empty
    local folder=$(dirname $dst)

    if [[ ! -d $folder ]]; then
      info "creating $folder"
      mkdir -p $folder
    fi

    ln -s "$src" "$dst"
    success "linked $src to $dst"
  fi
}

# Set $OS_NAME variable
if [[ "$OSTYPE" == "darwin"* ]]; then
  OS_NAME="OSX"
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
  OS_NAME="LINUX"
fi
