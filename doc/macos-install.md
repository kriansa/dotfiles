# Install homebrew

Run the command in the terminal:

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

## Link pinentry-gui to /usr/local/bin

```
sudo ln -s $(which pinentry-gui) /usr/local/bin/pinentry-gui
```

## Install packages

brew install --cask 1password corretto@21 font-iosevka postman zoom spotify linearmouse stats \
  whatsapp ghostty slack hammerspoon

brew install asdf bat cowsay duf eza fd fish fzf gh jq neovim tree-sitter-cli awscli \
  rg shellcheck tmux vivid htop pinentry-mac git-delta gnupg

## Install podman

brew install podman docker-compose
sudo podman-mac-helper install

## Kubernetes-related packages

brew install kubectx krew go
