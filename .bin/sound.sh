#!/bin/sh


SINK=$( pactl list short sinks | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,' | head -n 1 )
NOW=$( pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,' )
num=$(( NOW / 10 ))

[ "$(pactl list sinks | grep Mute | awk '{$1=$1};1')" = "Mute: yes" ] &&
    echo ' <icon=mutedspeaker.xpm/> <fc=#181818,#cc8c3c>' "$NOW% </fc> " &&
    exit 0


case $num in
  0)  icon='<icon=vol0.xpm/>' ;;
  1)  icon='<icon=vol1.xpm/>' ;;
  2)  icon='<icon=vol1.xpm/>' ;;
  3)  icon='<icon=vol1.xpm/>' ;;
  4)  icon='<icon=vol2.xpm/>' ;;
  5)  icon='<icon=vol2.xpm/>' ;;
  6)  icon='<icon=vol2.xpm/>' ;;
  7)  icon='<icon=vol3.xpm/>' ;;
  8)  icon='<icon=vol3.xpm/>' ;;
  9)  icon='<icon=vol3.xpm/>' ;;
  10) icon='<icon=vol.xpm/>'  ;;
  *)  icon='<icon=vol.xpm/>'  ;;
esac

bar=" <fc=#181818,#73c936>$icon"

echo "$bar" "$NOW%" "</fc> "

exit 0
