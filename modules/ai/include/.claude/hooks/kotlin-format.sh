#!/usr/bin/env bash

# Read file_path from stdin JSON
file_path=$(python3 -c "import sys, json; print(json.load(sys.stdin).get('tool_input', {}).get('file_path', ''))")

# Only run for Kotlin files
if [[ ! -f "$file_path" ]] || [[ "$file_path" != *.kt ]]; then
  exit 0
fi

# Run ktlintFormat and capture output
output=$(./gradlew ktlintFormat --warning-mode=none 2>&1)
exit_code=$?

# If ktlint failed, show violations to stderr and exit 2 (blocking error)
if [[ $exit_code -ne 0 ]]; then
  echo "ktlint violations:" >&2
  echo "$output" | grep -E "\.kt:[0-9]+:[0-9]+" | head -20 >&2
  exit 2
fi
