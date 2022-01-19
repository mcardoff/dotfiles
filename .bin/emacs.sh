#!/bin/sh

OUTPUT=`dir -1 ~/.config/eprofiles/ | dmenu`

if [ -n "$OUTPUT" ]; then
    emacs --with-profile $OUTPUT
fi 
