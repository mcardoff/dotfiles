#!/bin/sh
PAT="/home/mcard/school/.misc/Books/"
OUTPUT=$(dir "$PAT"/*.pdf | awk -F '/' '{print $NF}' | dmenu)

if [ -n "$OUTPUT" ]; then
    zathura "$PAT$OUTPUT" &
fi
