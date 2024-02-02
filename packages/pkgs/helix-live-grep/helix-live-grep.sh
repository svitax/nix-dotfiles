#!/usr/bin/env bash

# live-grep: interactive search, output is "file:line" pairs
FILE_PATHS=$(live-grep --exit-on-execution | tr '\n' ' ' | sed 's/ *$//')

# Get ID of the pane above in wezterm, which should be Helix
# HELIX_PANE_ID=$(wezterm cli get-pane-direction Up)

if [[ -n "$FILE_PATHS" ]]; then
  # Focus the pane containing Helix editor
  # zellij action toggle-floating-panes
  zellij action move-focus up

  # Send ":" to start command input in Helix
  # wezterm cli send-text --pane-id "$HELIX_PANE_ID" --no-paste ":"
  zellij action write-chars ":"

  # Send the "open" command with file path(s) to the pane
  # wezterm cli send-text --pane-id "$HELIX_PANE_ID" "open $FILE_PATHS"
  zellij action write-chars "open $FILE_PATHS"

  # Simulate 'Enter' key to execute the command
  # printf "\r" | wezterm cli send-text --pane-id "$HELIX_PANE_ID" --no-paste
  zellij action write-chars "$(printf '\r')"
fi
