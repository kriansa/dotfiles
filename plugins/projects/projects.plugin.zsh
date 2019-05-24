# Plugin path
PLUGIN_PATH=$0:A:h

# This lets you quickly jump into a project directory.
#
# Type:
#
#   c <tab>
#
# ...to autocomplete on all of your projects in the directories specified in
# `functions/_c`. Typically I'm using it like:
#
#    c holm<tab>/bo<tab>
#
# ...to quickly jump into holman/boom, for example.
function c {
  cd "$PROJECTS/$1"
}

# Wrap around mkproj bin so we can cwd into the new created project
function mkproj {
  command mkproj "$@" && cd "$2"
}

# Add this plugin path to the fpath completion list
fpath+=($PLUGIN_PATH/completions)

# 2. Set default env vars
# It is used by quick-cd (c)
: ${PROJECTS:=~/Projects}

# 3. Export the variables
export PROJECTS
export PATH="$PLUGIN_PATH/bin:$PATH"
