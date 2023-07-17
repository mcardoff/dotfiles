#!/bin/sh
FILE=$(shuf -n1 -e ~/.local/scripts/ascii/*)
N=$(shuf -n 1 -e 1 2 3 4 5 6 7 9)

printf "$(tput setaf $N)$(cat $FILE)$(tput sgr0)\n"
