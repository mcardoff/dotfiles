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
  0)  bar='<icon=wifi0.xpm/><fc=#ffffff>' ;;
  1)  bar='<icon=wifi1.xpm/><fc=#c92127>' ;;
  2)  bar='<icon=wifi1.xpm/><fc=#c92127>' ;;
  3)  bar='<icon=wifi2.xpm/><fc=#978216>' ;;
  4)  bar='<icon=wifi2.xpm/><fc=#978216>' ;;
  5)  bar='<icon=wifi3.xpm/><fc=#13377e>' ;;
  6)  bar='<icon=wifi3.xpm/><fc=#13377e>' ;;
  7)  bar='<icon=wifi3.xpm/><fc=#13377e>' ;;
  8)  bar='<icon=wifi4.xpm/><fc=#60bb46>' ;;
  9)  bar='<icon=wifi4.xpm/><fc=#60bb46>' ;;
  10) bar='<icon=wifi5.xpm/><fc=#60bb46>' ;;
  *)  bar='----!!----<fc=#ffffff>' ;;
esac

echo $bar "$stngth%" $essid "</fc>"

exit 0
