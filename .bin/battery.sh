#!/bin/sh
BAT="/sys/class/power_supply/BAT0/"
MAX=$(cat $BAT/energy_full)
CUR=$(cat $BAT/energy_now)
PER=$(( 100 * CUR / MAX ))
num=$(($PER * 8 / 100))

case $num in
  0)  bar='<icon=battery_on_0.xpm/><fc=#8b3622>' ;;
  1)  bar='<icon=battery_on_1.xpm/><fc=#8b3622>' ;;
  2)  bar='<icon=battery_on_2.xpm/><fc=#8b3622>' ;;
  3)  bar='<icon=battery_on_3.xpm/><fc=#cc8c3c>' ;;
  4)  bar='<icon=battery_on_4.xpm/><fc=#cc8c3c>' ;;
  5)  bar='<icon=battery_on_5.xpm/><fc=#cc8c3c>' ;;
  6)  bar='<icon=battery_on_6.xpm/><fc=#73c936>' ;;
  7)  bar='<icon=battery_on_7.xpm/><fc=#73c936>' ;;
  8)  bar='<icon=battery_on_8.xpm/><fc=#73c936>' ;;
  *)  bar='Error in Battery<fc=#73c936>' ;;
esac

echo $bar "$PER%</fc>" 
