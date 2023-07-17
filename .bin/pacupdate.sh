#!/bin/sh

cupd=$(checkupdates | wc -l)
echo "<icon=pacman.xpm/> <box type=Bottom width=2 mb=2 color=#ffdd33> <fc=#ffdd33>$cupd update(s) </fc></box> "
