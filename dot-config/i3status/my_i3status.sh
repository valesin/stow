#!/bin/bash
# This script merges output from i3status and my_custom_script.sh.
# It reads one line at a time from each and combines them.
#
# Make sure both i3status and my_custom_script.sh are installed/executable.
#
# The script uses process substitution to run both commands concurrently
# and the "paste" command to combine their outputs line by line.
paste <(i3status) <(. ~/Documents/Scripts/Pomo/pomo_timer.sh) | while IFS=$'\t' read -r i3_line custom_line
do
    # You can change the order or add additional formatting here.
    echo "$custom_line | $i3_line"
done
