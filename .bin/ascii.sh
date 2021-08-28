#!/bin/bash
NC=`tput sgr0`
N=$(shuf -i 1-10 -n 1)
COL=`tput setaf $N`
FILE=$(shuf -n1 -e ~/.bin/ascii/*)
CONTENT=`cat $FILE`
WCOLORS=${COL}${CONTENT}${NC}
force_color_prompt=yes
printf "${WCOLORS}"'\n' # prints a boi
