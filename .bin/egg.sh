#!/bin/sh

# change colors from gay to nongay or other way around
# check xmonad.hs to see if we are gay or not

gayq=$(grep "gay = " $XMONAD_CONFIG_DIR/xmonad.hs | tail -c5)
if [ $gayq == "alse" ]; then
    # change wallpaper, False -> True in xmonad.hs
    sed -i 's/gay = False/gay = True/g' $XMONAD_CONFIG_DIR/xmonad.hs
    sed -i 's/\*.color/!\*.reg/g' $XRESOURCES
    sed -i 's/!\*.gay/\*.color/g' $XRESOURCES 
    feh --bg-scale ~/Pictures/biancabackground.png
    # echo "not gay >:("
else
    sed -i 's/gay = True/gay = False/g' $XMONAD_CONFIG_DIR/xmonad.hs
    sed -i 's/\*.color/!\*.gay/g' $XRESOURCES
    sed -i 's/!\*.reg/\*.color/g' $XRESOURCES 
    feh --bg-scale ~/Pictures/background.png
    # echo "gay :)"
fi

xrdb -I$HOME ~/.Xresources
xmonad --recompile
xmonad --restart
