#!/bin/sh
BRUH=$(curl $1 | grep downloadUrl | sed "s/downloadUrl: //g" | sed 's/^ *//g' | tr ',' '\n' | tr '"' '\n' | sed 's/ /%20/g')
wget $BRUH
