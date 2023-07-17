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
  0)  bar='<box type=Bottom width=2 mb=2 color=#ffffff><icon=wifi0.xpm/><fc=#ffffff>' ;;
  1)  bar='<box type=Bottom width=2 mb=2 color=#8b3622><icon=wifi1.xpm/><fc=#8b3622>' ;;
  2)  bar='<box type=Bottom width=2 mb=2 color=#8b3622><icon=wifi1.xpm/><fc=#8b3622>' ;;
  3)  bar='<box type=Bottom width=2 mb=2 color=#cc8c3c><icon=wifi2.xpm/><fc=#cc8c3c>' ;;
  4)  bar='<box type=Bottom width=2 mb=2 color=#cc8c3c><icon=wifi2.xpm/><fc=#cc8c3c>' ;;
  5)  bar='<box type=Bottom width=2 mb=2 color=#ffdd33><icon=wifi3.xpm/><fc=#ffdd33>' ;;
  6)  bar='<box type=Bottom width=2 mb=2 color=#ffdd33><icon=wifi3.xpm/><fc=#ffdd33>' ;;
  7)  bar='<box type=Bottom width=2 mb=2 color=#ffdd33><icon=wifi3.xpm/><fc=#ffdd33>' ;;
  8)  bar='<box type=Bottom width=2 mb=2 color=#73c936><icon=wifi4.xpm/><fc=#73c936>' ;;
  9)  bar='<box type=Bottom width=2 mb=2 color=#73c936><icon=wifi4.xpm/><fc=#73c936>' ;;
  10) bar='<box type=Bottom width=2 mb=2 color=#73c936><icon=wifi5.xpm/><fc=#73c936>' ;;
  *)  bar='<box type=Bottom width=2 mb=2 color=#ffffff>----!!----<fc=#ffffff>' ;;
esac

echo $bar "$stngth%" $essid "</fc></box>"

exit 0
