#!/usr/bin/env sh

pkill -9 polybar
while pgrep -x polybar >/dev/null; do
    sleep 0.2
done

for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar -r main &
done
