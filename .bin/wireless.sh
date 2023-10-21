#!/bin/sh

iwconfig wlp2s0 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

essid=$(iwconfig wlp2s0 | awk -F '"' '/ESSID/ {print $2}')
stngth=$(iwconfig wlp2s0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1)

[ -z "$stngth" ] && echo "<fc=#181818,#8b3622> Disconnected! </fc> " && exit 0

bars=$(($stngth / 10))

color='#73c936'

case $bars in
  0)  icon='<icon=wifi0.xpm/>' ;;
  1)  icon='<icon=wifi1.xpm/>' ;;
  2)  icon='<icon=wifi1.xpm/>' ;;
  3)  icon='<icon=wifi2.xpm/>' ;;
  4)  icon='<icon=wifi2.xpm/>' ;;
  5)  icon='<icon=wifi3.xpm/>' ;;
  6)  icon='<icon=wifi3.xpm/>' ;;
  7)  icon='<icon=wifi3.xpm/>' ;;
  8)  icon='<icon=wifi4.xpm/>' ;;
  9)  icon='<icon=wifi4.xpm/>' ;;
  10) icon='<icon=wifi5.xpm/>' ;;
  *)  icon='----!!----' ;;
esac

bar="$icon <fc=#181818,$color>"

echo $bar "$stngth%" $essid "</fc> "

exit 0
