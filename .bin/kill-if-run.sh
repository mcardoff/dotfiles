#!/bin/bash

pid=$(pgrep $1)

if [ -n "$pid" ] ; then # if it is running, kill it
    kill $pid
fi

# run it again
if [ -n "$2" ] ; then
    $1 --$2 &
else
    $1 &
fi
