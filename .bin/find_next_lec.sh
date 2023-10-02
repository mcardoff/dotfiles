#!/bin/bash

#INPUT FORMAT:
# $1 = SEMESTER
# $2 = CLASS

BIGGEST=$(dir -1 /home/mcard/school/$1/$2/Lab_L*.tex 2> /dev/null |
	      sed 's/^.*Lecture_//' |
	      sed 's/.tex//g' | tail -1)

[[ -z "$BIGGEST" ]] && BIGGEST=0

NEXTNUM=$(( 10#$BIGGEST + 1 ))
FINAL="$NEXTNUM"

[[ $NEXTNUM -lt 10 ]] && FINAL="0$NEXTNUM"

# Print next file name
printf "/home/mcard/school/$1/$2/Lab_Lecture_$FINAL.tex"
