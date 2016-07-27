# If needed, we define PLUGIN_PATH here
PLUGIN_PATH=$0:A:h

# 1. Exports
# This is a part dedicated to exporting env variables into our shell
# This is where $PATH concats should have done
export DUMMY_HOME="$HOME/.dotfiles"
export PATH="$PATH:$DUMMY_HOME"

# 2. Load commands/settings
# Here you load any file you want, such as commands
if (( $+commands[hello] )); then
  eval "function hello { echo 'hahaha!' }"
fi

# 3. Functions
# Here we can set any functions
# This function does bla, bla and blah
#
# Type:
#
#   dummy
#
# To see that it does abolutely nothing
#
function dummy {
  echo "dummy plugin"
}

# 4. Aliases
# Plugin-related aliases
alias dummy_alias='echo "hey check it out, I am a dummy plugin"'
