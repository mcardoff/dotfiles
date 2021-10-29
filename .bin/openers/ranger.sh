#!/bin/bash
pid=$(pgrep ranger)

# echo $pid

if [ -n "$pid" ] ; then
    echo $pid
else
    alacritty -t Ranger -e ranger
fi
		     
