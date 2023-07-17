#!/bin/sh


SINK=$( pactl list short sinks | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,' | head -n 1 )
NOW=$( pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,' )
num=$(( NOW / 10 ))

[ "$(pactl list sinks | grep Mute | awk '{$1=$1};1')" = "Mute: yes" ] &&
    echo '<box type=Bottom width=2 mb=2 color=#cc8c3c><icon=mutedspeaker.xpm/><fc=#cc8c3c>' "$NOW%</fc></box>" &&
    exit 0

bar='<box type=Bottom width=2 mb=2 color=#73c936><fc=#73c936>'

case $num in
  0)  bar='${bar}<icon=vol0.xpm/>' ;;
  1)  bar='${bar}<icon=vol1.xpm/>' ;;
  2)  bar='${bar}<icon=vol1.xpm/>' ;;
  3)  bar='${bar}<icon=vol1.xpm/>' ;;
  4)  bar='${bar}<icon=vol2.xpm/>' ;;
  5)  bar='${bar}<icon=vol2.xpm/>' ;;
  6)  bar='${bar}<icon=vol2.xpm/>' ;;
  7)  bar='${bar}<icon=vol3.xpm/>' ;;
  8)  bar='${bar}<icon=vol3.xpm/>' ;;
  9)  bar='${bar}<icon=vol3.xpm/>' ;;
  10) bar='${bar}<icon=vol.xpm/>'  ;;
  *)  bar='${bar}<icon=vol.xpm/>'  ;;
esac

echo $bar "$NOW%" "</fc></box>"

exit 0
