#!/bin/sh

OUTPUT=`dir -1 ~/eprofiles/ | dmenu -sb '#8b3622' -sf '#FAFAFA' -nb '#282828' -nf '#FAFAFA'`

if [ -n "$OUTPUT" ]; then
    emacs --with-profile $OUTPUT
fi 
