#!/bin/sh


SINK=$( pactl list short sinks | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,' | head -n 1 )
NOW=$( pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,' )
num=$(( NOW / 10 ))

[ "$(pactl list sinks | grep Mute | awk '{$1=$1};1')" = "Mute: yes" ] &&
    echo '<icon=mutedspeaker.xpm/><fc=#cc8c3c>' "$NOW%</fc>" &&
    exit 0

case $num in
  0)  bar='<icon=vol0.xpm/>' ;;
  1)  bar='<icon=vol1.xpm/>' ;;
  2)  bar='<icon=vol1.xpm/>' ;;
  3)  bar='<icon=vol1.xpm/>' ;;
  4)  bar='<icon=vol2.xpm/>' ;;
  5)  bar='<icon=vol2.xpm/>' ;;
  6)  bar='<icon=vol2.xpm/>' ;;
  7)  bar='<icon=vol3.xpm/>' ;;
  8)  bar='<icon=vol3.xpm/>' ;;
  9)  bar='<icon=vol3.xpm/>' ;;
  10) bar='<icon=vol.xpm/>'  ;;
  *)  bar='<icon=vol.xpm/>'  ;;
esac

echo $bar "$NOW%"

exit 0
