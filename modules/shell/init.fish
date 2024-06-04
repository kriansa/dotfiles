# This sets the color scheme of bat for a more pleasant in light terminals
set --global --export BAT_THEME OneHalfLight
alias cat=bat

# Disable cowsay for ansible
set --global --export ANSIBLE_NOCOWS 1

# Set the function globally instead of keeping it under `functions`. Under that path it can be
# easily autoloaded by fish runtime, however there can be only one `fish_greeting.fish` for the
# entire setup. That can be conflicting if we happen to install some other package that also provide
# that same file. So the solution instead is to force that function to be loaded before fish needs
# to autoload it, so that it won't matter if any other packages also provide it -- it will always
# execute this function right here.
function fish_greeting
  test -n "$no_greeting" && return
  test -r "$HOME/Documents/Fortunes" || return
  cat $HOME/Documents/Fortunes/*.txt | sort --random-sort | head -1 | cowsay
end

if status is-interactive
  # Add navigational helpers
  # (these functions couldn't be in their dedicated files because they would have weird names)
  function ...
  ../..
  end

  function ....
  ../../..
  end
end

# Because we'reoverriding the `rm` command, in some cases we want to use the original one, such as
# on `fish-async-prompt` cleanup. Here, if it uses the aliased `rm`, it will start filling up the
# system trash with a lot of tempfiles on every terminal exit.
#
# TODO: This should be fixed upstream. Remove when fixed.
# See: https://github.com/acomagu/fish-async-prompt
function __async_prompt_tmpdir_cleanup --on-event fish_exit
  command rm -rf "$__async_prompt_tmpdir"
end
