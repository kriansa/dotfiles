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

cd $DOTFILES_PATH/modules/work/data && brew bundle

## Install podman

sudo podman-mac-helper install

## Kubernetes-related packages

brew install kubectx krew go
