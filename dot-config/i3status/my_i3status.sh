#!/bin/sh
# Shell script to prepend i3status with the output from my_custom_script.sh,
# and every 10 seconds send SIGUSR1 to i3status

# Start background task to send SIGUSR1 every 10 seconds
(
  while true; do
    sleep 15
    killall -SIGUSR1 i3status
  done
) &

# Read the output from i3status line by line
i3status | while read line; do
    custom_output=$(~/Documents/Scripts/pomo/pomo --status)
    echo "$custom_output | $line" || exit 1
done
