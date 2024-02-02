#!/usr/bin/env bash

# TODO: find a way to use nix-colors here
# Colors
GREEN="#b8bb26"
PURPLE="#d3869b"

# Key bindings
COPY_FILE_PATH='ctrl-y:execute(echo -n {1}:{2} | wl-copy),alt-a:select-all,alt-d:kill-word,alt-t:toggle-all,ctrl-j:accept,ctrl-k:kill-line,ctrl-n:down,ctrl-p:up,up:previous-history,down:next-history'
KEYS="$COPY_FILE_PATH"

# Optional flag for execution and exit behavior
if [[ $1 == '--exit-on-execution' ]]; then
  KEYS="$KEYS+abort"
  shift # remove the flag from the arguments so it's not passed to the 'rg' command
fi

IFS=$'\n' readarray -t selected_matches < <(
  rg --color=always --line-number --no-heading --smart-case "${*:-}" |
    fzf --ansi \
      --border \
      --color "hl+:$GREEN:reverse,hl:$PURPLE:reverse" \
      --delimiter ':' \
      --height '100%' \
      --multi \
      --print-query --exit-0 \
      --preview 'bat {1} --highlight-line {2}' \
      --preview-window 'right,+{2}+3/3,~3' \
      --scrollbar '▍' \
      --reverse \
      --tiebreak 'end' \
      --bind "$KEYS"
)

# Print the file path and line number of each match
for line in "${selected_matches[@]:1}"; do
  file=$(echo "$line" | cut -d: -f1)
  line_number=$(echo "$line" | cut -d: -f2)
  echo "$file:$line_number"
done
