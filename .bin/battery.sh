#!/bin/sh
BAT="/sys/class/power_supply/BAT0/"
STAT=$(cat $BAT/status)
PER=$(( 100 * $(cat $BAT/energy_now) / $(cat $BAT/energy_full) ))
num=$(( PER * 8 / 100 ))

bolt="idle"
pat='/home/mcard/.config/xmonad/xpm/batt/'


{ [ "$STAT" = "Charging" ] || [ "$STAT" = "Unknown" ] ;} && bolt="charging"

color=$(grep -m 1 "\". c #" $pat$bolt$num.xpm | sed 's/\"//g' | sed 's/. c //' | sed 's/,//')

echo "<icon=batt/$bolt$num.xpm/><fc=$color>" "$PER%</fc>" 

