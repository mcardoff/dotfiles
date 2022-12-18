#!/bin/bash

#INPUT FORMAT:
# $1 = SEMESTER
# $2 = CLASS

BIGGEST=$(dir -1 /home/mcard/school/$1/$2/*.tex 2> /dev/null|
	      awk -F'\n' '{print $1 $2 $3 $4}' |
	      awk -F'[_.tex]' '{print $5}' | tail -1)
# echo $BIGGEST

[[ -z "$BIGGEST" ]] && BIGGEST=0

NEXTNUM=$((10#$BIGGEST + 1))
FINAL="$NEXTNUM"

[[ $NEXTNUM -lt 10 ]] && FINAL="0$NEXTNUM"

printf "$FINAL"
