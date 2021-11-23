#!/bin/sh
BAT="/sys/class/power_supply/BAT0/"

STAT=$(cat $BAT/status)
PER=$(( 100 * $(cat $BAT/energy_now) / $(cat $BAT/energy_full) ))
num=$(( PER * 8 / 100 ))
bolt="idle"

pat='/home/mcard/.config/xmonad/xpm/batt/'

[ "$STAT" = "Unknown" ] && bolt="charging"

color=$(cat $pat$bolt"_"$num.xpm | grep -m 1 "\". c #" | sed 's/\"//g' | sed 's/. c //' | sed 's/,//')

# echo $color
bar="<icon=batt/"$bolt"_"$num".xpm/><fc=$color>"

echo "$bar $PER%</fc>" 
