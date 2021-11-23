#!/bin/sh
FILE=$(shuf -n1 -e ~/.bin/ascii/*)
N=$(shuf -n 1 -e 1 2 3 4 5 6 7 9)
# echo $N
printf "$(tput setaf $N)$(cat "$FILE")$(tput sgr0)\n"
