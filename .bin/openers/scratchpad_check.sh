#!/bin/bash
# Logic for dropdown term and emacs w/ server
# need it to be bigger than one line, pipe through wc -l, as ps will list 
# if it equals one, it is just the grep process, we do not need to run again

# Ranger 
[ -z "$(pgrep ranger)" ] &&
    st -c "Ranger" -t "Ranger" -e ranger &

# dropdown term
[ $(ps -ef | grep dropterm | wc -l) -eq 1 ] &&
    st -c "dropterm" &

# vim term
[ $(ps -ef | grep vim | wc -l) -eq 1 ] &&
    st -c "vim" -t "vim" -e vim &

# emacs running server
# [ $(ps -ef | grep notepad | wc -l) -eq 1 ] &&
    # emacs -T notepad --eval="(unless (boundp 'server-process) (server-start))" &

# feh with schedule adds 2 instead of 1, but the basic is the same
[ $(ps -ef | grep Schedule | wc -l) -eq 1 ] &&
    feh ~/Pictures/schedule.png --title "Schedule" &
