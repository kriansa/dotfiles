# Avoid flicker from claude code
# See: https://github.com/anthropics/claude-code/issues/1913
set --global --export ENABLE_INCREMENTAL_TUI true

# Disable auto updater, /bug command, error reporting and telemetry
set --global --export CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC 1

# Remove need for janky wrapper on .local/bin
set --global --export DISABLE_INSTALLATION_CHECKS 1

# Enable tool search always
set --global --export ENABLE_TOOL_SEARCH true

# Set which code agent we're using
set --global --export CODE_AGENT claude
