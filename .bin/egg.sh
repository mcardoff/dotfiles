#!/bin/sh

# change colors from gay to nongay or other way around
# check xmonad.hs to see if we are gay or not

gayq=$(grep "gay = " $XMONAD_CONFIG_DIR/xmonad.hs | tail -c5)
if [ $gayq == "alse" ]; then
    # change wallpaper, False -> True in xmonad.hs
    sed -i 's/gay = False/gay = True/g' $XMONAD_CONFIG_DIR/xmonad.hs
    sed -i 's/\*.color/!\*.reg/g' $XDG_CONFIG_HOME/X11/xresources
    sed -i 's/!\*.gay/\*.color/g' $XDG_CONFIG_HOME/X11/xresources
    xloadimage -onroot -fullscreen ~/Pictures/biancabackground.png
    # echo "not gay >:("
else
    sed -i 's/gay = True/gay = False/g' $XMONAD_CONFIG_DIR/xmonad.hs
    sed -i 's/\*.color/!\*.gay/g' $XDG_CONFIG_HOME/X11/xresources
    sed -i 's/!\*.reg/\*.color/g' $XDG_CONFIG_HOME/X11/xresources
    xloadimage -onroot -fullscreen ~/Pictures/background.png
    # echo "gay :)"
fi

xrdb -I$HOME ~/.config/X11/xresources
xmonad --recompile
xmonad --restart