#!/bin/sh
xrandr --output DP-2-3 --right-of eDP-1 --mode 1920x1080 ||
    xrandr --output DP-1-3 --right-of eDP-1 --mode 1920x1080
source /home/mcard/.local/scripts/cisbg.sh
