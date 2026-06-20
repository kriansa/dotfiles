#!/bin/bash
input=$(cat)

# --- Data extraction ---
cwd_raw=$(echo "$input" | jq -r '.workspace.current_dir')
cwd="${cwd_raw/#$HOME/~}"
model=$(echo "$input" | jq -r '.model.display_name')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
five_pct=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
week_pct=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')

# --- Git branch ---
cd "$cwd_raw" 2>/dev/null
branch=$(git --no-optional-locks branch --show-current 2>/dev/null)

# --- Progress bar builder (10 chars wide) ---
make_bar() {
    local pct="$1"
    local filled=$(printf "%.0f" "$(echo "$pct * 10 / 100" | bc -l 2>/dev/null || echo 0)")
    [ "$filled" -gt 10 ] && filled=10
    local empty=$((10 - filled))
    local bar=""
    for i in $(seq 1 $filled); do bar="${bar}█"; done
    for i in $(seq 1 $empty); do bar="${bar}░"; done
    printf "%s" "$bar"
}

# --- Line 1: path + branch ---
if [ -n "$branch" ]; then
    printf "📁 %s  🌿 %s\n" "$cwd" "$branch"
else
    printf "📁 %s\n" "$cwd"
fi

# --- Line 2: model, context, rate limits ---
line2=""

# Model
line2="🤖 ${model}"

# Context window
if [ -n "$used_pct" ]; then
    bar=$(make_bar "$used_pct")
    used_int=$(printf "%.0f" "$used_pct")
    line2="${line2}     🧠 [${bar}] ${used_int}%"
fi

# Rate limits
rate_parts=""
if [ -n "$five_pct" ]; then
    bar=$(make_bar "$five_pct")
    five_int=$(printf "%.0f" "$five_pct")
    rate_parts="${rate_parts}     ⏱ 5h [${bar}] ${five_int}%"
fi
if [ -n "$week_pct" ]; then
    bar=$(make_bar "$week_pct")
    week_int=$(printf "%.0f" "$week_pct")
    rate_parts="${rate_parts}     📅 7d [${bar}] ${week_int}%"
fi

if [ -n "$rate_parts" ]; then
    line2="${line2}     ${rate_parts}"
fi

printf "%s\n" "$line2"
