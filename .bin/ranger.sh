#!/bin/bash
pid=$(pgrep ranger)

# echo $pid

if [ -n "$pid" ] ; then
    echo $pid
    kill $pid
fi

(alacritty -t Ranger -e ranger &)
		     
