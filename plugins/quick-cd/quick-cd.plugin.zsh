# 1. Functions
#
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

# 2. Set default env vars
# Default $PROJECTS is ~/Projects
# It is used by quick-cd (c)
: ${PROJECTS:=~/Projects}
