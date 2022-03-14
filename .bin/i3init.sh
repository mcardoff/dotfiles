#!/bin/bash

~/.bin/kill-if-run.sh compton &
# ~/.bin/kill-if-run.sh lxappearance &
gayq=$(grep "gay = " $XMONAD_CONFIG_DIR/xmonad.hs | tail -c5)
if [ $gayq == "alse" ]; then
    xloadimage -onroot -fullscreen ~/Pictures/biancabackground.png
else
    xloadimage -onroot -fullscreen ~/Pictures/background.png
fi
