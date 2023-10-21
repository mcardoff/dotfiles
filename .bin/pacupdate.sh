#!/bin/sh

cupd=$(checkupdates | wc -l)
echo "<icon=pacman.xpm/> <fc=#181818,#cc8c3c> $cupd update(s) </fc>"
