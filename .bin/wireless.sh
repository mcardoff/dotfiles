#!/bin/sh

iwconfig wlp2s0 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

essid=$(~/.bin/essid.sh)
stngth=`iwconfig wlp2s0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1`

[ -z "$stngth" ] && echo "Disconnected!" && exit 0

bars=`expr $stngth / 10`

# case $bars in
#   0)  bar='----------' ;;
#   1)  bar='/---------' ;;
#   2)  bar='//--------' ;;
#   3)  bar='///-------' ;;
#   4)  bar='////------' ;;
#   5)  bar='/////-----' ;;
#   6)  bar='//////----' ;;
#   7)  bar='///////---' ;;
#   8)  bar='////////--' ;;
#   9)  bar='/////////-' ;;
#   10) bar='//////////' ;;
#   *)  bar='----!!----' ;;
# esac

case $bars in
  0)  bar='<icon=wifi0.xpm/><fc=#ffffff>' ;;
  1)  bar='<icon=wifi1.xpm/><fc=#8b3622>' ;;
  2)  bar='<icon=wifi1.xpm/><fc=#8b3622>' ;;
  3)  bar='<icon=wifi2.xpm/><fc=#cc8c3c>' ;;
  4)  bar='<icon=wifi2.xpm/><fc=#cc8c3c>' ;;
  5)  bar='<icon=wifi3.xpm/><fc=#ffdd33>' ;;
  6)  bar='<icon=wifi3.xpm/><fc=#ffdd33>' ;;
  7)  bar='<icon=wifi3.xpm/><fc=#ffdd33>' ;;
  8)  bar='<icon=wifi4.xpm/><fc=#73c936>' ;;
  9)  bar='<icon=wifi4.xpm/><fc=#73c936>' ;;
  10) bar= '<icon=wifi.xpm/><fc=#73c936>' ;;
  *)  bar='----!!----' ;;
esac

echo $bar "$stngth%" $essid "</fc>"

exit 0
