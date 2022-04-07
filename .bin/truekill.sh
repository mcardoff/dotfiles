#!/bin/sh
OUTPUT=$(dmenu -p "KILL>")

if [ -n "$OUTPUT" ]; then
    kill $(pidof "$OUTPUT") &
fi
