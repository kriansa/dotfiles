# This file (config.fish) is loaded after all conf.d ones -- use it as the last resource for a
# late-loaded configuration.

# Add ~/.bin to path as the first in the list so it overrides existing system executables
fish_add_path --path --move --global $HOME/.bin
