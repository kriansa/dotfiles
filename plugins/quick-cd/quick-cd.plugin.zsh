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
#
function c {
  cd "$PROJECTS/$1"
}

# Add this plugin path to the fpath completion list
fpath+=($PLUGIN_PATH)

# 2. Set default env vars
# Default $PROJECTS is ~/Projects
# It is used by quick-cd (c)
: ${PROJECTS:=~/Projects}

# 3. Ensure we export the $PROJECTS variable
export PROJECTS
