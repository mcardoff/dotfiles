#!/bin/sh

#INPUT FORMAT:
# $1 = SEMESTER
# $2 = CLASS

BIGGEST=$(dir -1 /home/mcard/school/$1/$2/*.tex |
	  awk -F'\n' '{print $1 $2 $3 $4}' |
	  awk -F'[_.tex]' '{print $5}' | tail -1)
# echo $BIGGEST

NEXTNUM=$(($BIGGEST + 1))
FINAL="$NEXTNUM"

[[ $NEXTNUM -lt 9 ]] && FINAL="0$NEXTNUM"

printf "$FINAL"