#!/bin/sh
NC='\033[0m'
YELLOW='\033[01;33m'
RED='\033[01;31m'
FILE=$(shuf -n1 -e ~/.bin/ascii/*)
CONTENT=`cat $FILE`
WCOLORS=${YELLOW}${CONTENT}${NC}
force_color_prompt=yes
printf "${WCOLORS}"'\n' # prints a boi
