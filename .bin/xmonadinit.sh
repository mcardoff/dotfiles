#!/bin/sh

exec nm-applet &

gayq=$(grep "gay = " $XMONAD_CONFIG_DIR/xmonad.hs | tail -c5)

if [ $gayq == "alse" ]; then
    /home/mcard/.bin/cisbg.sh &
else
    /home/mcard/.bin/bibg.sh &
fi

[[ -f /home/mcard/.config/X11/xresources ]] && xrdb -merge -I$HOME /home/mcard/.config/X11/xresources

xinput set-prop 12 186 0
