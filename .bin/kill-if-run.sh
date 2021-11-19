#!/bin/sh

[ -n "$(pgrep $1)" ] && kill $(pgrep $1)

# run it again
if [ -n "$2" ] ; then
    $1 --$2 &
else
    $1 &
fi
