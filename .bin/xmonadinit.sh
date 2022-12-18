#!/bin/sh

exec nm-applet &

/home/mcard/.bin/cisbg.sh &

[[ -f /home/mcard/.config/X11/xresources ]] && xrdb -merge -I$HOME /home/mcard/.config/X11/xresources

xinput set-prop 12 186 0
