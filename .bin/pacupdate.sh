#!/bin/sh

cupd=$(checkupdates | wc -l)
echo "<icon=pacman.xpm/> <fc=#181818,#ffdd33> $cupd update(s) </fc>"
