#!/bin/bash

OUTPUT=$(find ~/Org/Agenda/*.org -printf '%T@ %p\n' | sort -n | tail -1 | cut -f2- -d" ")

printf "$OUTPUT"
