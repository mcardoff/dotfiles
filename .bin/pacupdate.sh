#!/bin/sh

cupd=$(checkupdates | wc -l)
color=$(grep -m 1 "\". c #" '/home/mcard/.config/xmonad/xpm/pacman.xpm' | sed 's/\"//g' | sed 's/. c //' | sed 's/,//')
echo "<icon=pacman.xpm/> <fc=#181818,$color> $cupd update(s) </fc>"
