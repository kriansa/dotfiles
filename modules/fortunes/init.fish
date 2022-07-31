# Disable cowsay for ansible
set --global --export ANSIBLE_NOCOWS 1

# Set the function globally instead of keeping it under `functions`. Under that path it can be
# easily autoloaded by fish runtime, however there can be only one `fish_greeting.fish` for the
# entire setup. That can be conflicting if we happen to install some other package that also provide
# that same file. So the solution instead is to force that function to be loaded before fish needs
# to autoload it, so that it won't matter if any other packages also provide it -- it will always
# execute this function right here.
function fish_greeting
  cat $HOME/Documents/Fortunes/*.txt | sort --random-sort | head -1 | cowsay
end
