#!/bin/sh

OUTPUT=`find /home/mcard/school/ -type f -iname '*.pdf' | dmenu`

if [ -n "$OUTPUT" ]; then
    zathura $OUTPUT
fi
