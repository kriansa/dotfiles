# Install homebrew

Run the command in the terminal:

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Add the following file content to `/etc/paths.d/homebrew`:

```
/opt/homebrew/bin
/opt/homebrew/sbin
```

## Link pinentry-gui to /usr/local/bin

```
sudo ln -s $(which pinentry-gui) /usr/local/bin
```

## Install packages

brew install --cask 1password alacritty coretto font-iosevka-nerd-font \
  postman zoom deepl spotify unnaturalscrollwheels

brew install asdf bat cowsay duf eza fd fish fzf gh jq neovim pass pass-otp awscli \
  hammerspoon rg shellcheck tmux vivid htop pinentry-mac trash go gopls pre-commit

# Configure alacritty
