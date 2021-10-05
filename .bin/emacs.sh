#!/bin/sh

OUTPUT=$(dir -1 ~/eprofiles/ | dmenu -sb '#8b3622' -sf '#FAFAFA' -nb '#282828' -nf '#FAFAFA')

case "$OUTPUT" in
    regmacs)
	emacs --with-profile default
	;;

    doomacs)
	emacs --with-profile doom
	;;

    spacemacs)
	emacs --with-profile space
	;;
    
    evil)
	emacs --with-profile evil
	;;
    
    *)
	;;
esac
    
