#!/bin/sh

iwconfig wlp2s0 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

essid=$(~/.bin/essid.sh)
stngth=$(iwconfig wlp2s0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1)

[ -z "$stngth" ] && echo "Disconnected!" && exit 0

bars=$(($stngth / 10))

case $bars in
  0)  color='#ffffff' && icon='<icon=wifi0.xpm/>' ;;
  1)  color='#8b3622' && icon='<icon=wifi1.xpm/>' ;;
  2)  color='#8b3622' && icon='<icon=wifi1.xpm/>' ;;
  3)  color='#cc8c3c' && icon='<icon=wifi2.xpm/>' ;;
  4)  color='#cc8c3c' && icon='<icon=wifi2.xpm/>' ;;
  5)  color='#ffdd33' && icon='<icon=wifi3.xpm/>' ;;
  6)  color='#ffdd33' && icon='<icon=wifi3.xpm/>' ;;
  7)  color='#ffdd33' && icon='<icon=wifi3.xpm/>' ;;
  8)  color='#73c936' && icon='<icon=wifi4.xpm/>' ;;
  9)  color='#73c936' && icon='<icon=wifi4.xpm/>' ;;
  10) color='#73c936' && icon='<icon=wifi5.xpm/>' ;;
  *)  color='#ffffff' && icon='----!!----' ;;
esac

bar="<box type=Bottom width=2 mb=2 color=$color>$icon<fc=$color>"

echo $bar "$stngth%" $essid "</fc></box>"

exit 0
