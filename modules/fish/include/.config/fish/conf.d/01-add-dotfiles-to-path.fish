# Add the linked shell modules to the function and completions path
#
# This has to be done as early as possible so any file under `conf.d` can use functions only
# available at these paths
set --prepend fish_function_path $HOME/.config/fish/modules/functions
set --prepend fish_complete_path $HOME/.config/fish/modules/completions
