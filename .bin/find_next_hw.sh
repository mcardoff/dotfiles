#!/bin/sh

BIGGEST=$(dir -1 ~/school/SP22/$1/*.tex |
	  awk -F'\n' '{print $1 $2 $3 $4}' |
	  awk -F'[_.tex]' '{print $5}' | tail -1)

NEXTNUM=$(($BIGGEST + 1))
FINAL="0$NEXTNUM"

if [ $NEXTNUM -gt 9 ]; then
    FINAL="$NEXTNUM"
fi

printf $FINAL
