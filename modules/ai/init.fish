# Avoid flicker from claude code
# See: https://github.com/anthropics/claude-code/issues/1913
set --global --export ENABLE_INCREMENTAL_TUI true
