#!/bin/bash
# Claude Code status line: directory + git branch + model name + context remaining % + beads task
input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
dir_display=$(basename "$cwd")
model=$(echo "$input" | jq -r '.model.display_name')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
# Lets the task lookup find the task this session claimed, even while the
# session sits in the main checkout and the work happens in a worktree.
session_id=$(echo "$input" | jq -r '.session_id // empty')
# Lets the task lookup drop the title when the session is already named after it.
session_name=$(echo "$input" | jq -r '.session_name // empty')

branch=""
if git -C "$cwd" --no-optional-locks rev-parse --is-inside-work-tree > /dev/null 2>&1; then
  branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)
fi

task=$(bash "${XDG_DATA_HOME:-$HOME/.local/share}/beads-task/statusline-task.sh" "$cwd" "$session_id" "$session_name" 2>/dev/null)

DIM='\033[2m'
CYAN='\033[2;36m'
GREEN='\033[2;32m'
YELLOW='\033[2;33m'
MAGENTA='\033[2;35m'
RESET='\033[0m'

out="${DIM}${dir_display}${RESET}"
if [ -n "$branch" ]; then
  out="${out} ${GREEN}(${branch})${RESET}"
fi
out="${out} ${DIM}|${RESET} ${CYAN}${model}${RESET}"
if [ -n "$remaining" ]; then
  out="${out} ${DIM}|${RESET} ${YELLOW}${remaining}% left${RESET}"
fi
# Task last: it is the one field with no length bound, so anything after it
# would be the first thing a narrow terminal truncates.
if [ -n "$task" ]; then
  out="${out} ${DIM}|${RESET} ${MAGENTA}${task}${RESET}"
fi

printf "%b" "$out"
