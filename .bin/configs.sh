#!/bin/sh

OUTPUT=$(find -L ~/.config -maxdepth 2 -type f | dmenu -sb '#8b3622' -sf '#FAFAFA' -nb '#282828' -nf '#FAFAFA')

emacs $OUTPUT
