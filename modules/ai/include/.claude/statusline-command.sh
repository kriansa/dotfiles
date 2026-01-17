#!/bin/bash
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
dir_name=$(basename "$cwd")

cd "$cwd" 2>/dev/null || exit 0
branch=$(git --no-optional-locks branch --show-current 2>/dev/null)

if [ -n "$branch" ]; then
    echo "$dir_name ($branch)"
else
    echo "$dir_name"
fi
