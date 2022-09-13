#!/bin/sh
PAT="/home/mcard/school/misc/Books/"
OUTPUT=$(find $PAT -type f -iname '*.pdf' | awk -F '/' '{print $NF}' | dmenu)

if [ -n "$OUTPUT" ]; then
    zathura "$PAT$OUTPUT" &
fi
