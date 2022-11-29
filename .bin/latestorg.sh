#!/bin/bash

OUTPUT=$(dir -1 -t /home/mcard/Org/Agenda/*.org | head -n 1)

printf "$OUTPUT"
