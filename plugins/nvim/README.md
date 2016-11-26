# NVIM Plugin

Whenever there's an update to Ruby version, do the following actions:

1. Update `include/.config/nvim/bin/ruby` and `include/.config/nvim/bin/neovim-ruby-host` to set the `RBENV_VERSION` accordingly.
2. Update `include/config/nvim/packages.vim` and set `RBENV_VERSION` accordingly.
3. Install neovim gem to that ruby globally:
  > gem install neovim
4. Re-run packages post-install hooks by running (inside NVim):
  > PlugInstall!
