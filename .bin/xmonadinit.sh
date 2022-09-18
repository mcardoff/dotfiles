#!/bin/sh

gayq=$(grep "gay = " $XMONAD_CONFIG_DIR/xmonad.hs | tail -c5)

if [ $gayq == "alse" ]; then
    ~/.bin/cisbg.sh &
else
    ~/.bin/bibg.sh &
fi

[[ -f ~/.config/X11/xresources ]] && xrdb -merge -I$HOME ~/.config/X11/xresources

xinput set-prop 12 186 0
