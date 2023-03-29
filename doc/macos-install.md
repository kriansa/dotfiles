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

## Install packages

brew install --cask 1password alacritty coretto font-iosevka-nerd-font rectangle
brew install asdf bat cowsay duf exa fd fish fzf gh jq neovim pass pass-otp rg shellcheck tmux vivid
htop pinentry-mac trash go gopls

# Configure alacritty
