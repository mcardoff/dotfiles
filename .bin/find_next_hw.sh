#!/bin/bash

#INPUT FORMAT:
# $1 = SEMESTER
# $2 = CLASS

BIGGEST=$(dir -1 /home/mcard/school/$1/$2/*.tex |
	  awk -F'\n' '{print $1 $2 $3 $4}' |
	  awk -F'[_.tex]' '{print $5}' | tail -1)
# echo $BIGGEST

NEXTNUM=$((10#$BIGGEST + 1))
FINAL="$NEXTNUM"

[[ $NEXTNUM -lt 10 ]] && FINAL="0$NEXTNUM"

COURSE=$(printf $2 | sed 's/[A-Z]//g')
# echo $COURSE

# Print next file name
printf "Cardiff_${COURSE}_HW_$FINAL.tex"
