#!/bin/bash
# This script reads one line at a time from i3status and my_custom_script.sh concurrently
# and outputs them combined with the custom output coming first.

# Open file descriptor 3 for i3status output.
exec 3< <(i3status)
# Open file descriptor 4 for my_custom_script.sh output.
exec 4< <(./my_custom_script.sh)

while true; do
    if ! read -r i3_line <&3 || ! read -r custom_line <&4; then
         # If either command stops producing output, break the loop.
         break
    fi
    echo "$custom_line | $i3_line"
done
