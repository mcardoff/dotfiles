#!/bin/sh

dir=$(pwd)

# This is meant to deploy the dotfiles so that I can simply clone the repo and get it done

if [ "$(pwd | tail --bytes=9)" != "dotfiles" ]; then
    echo "Wrong directory detected, please run this script in the repository which it originates from"
    exit 1
fi

# DONE Copy wallpaper into ~/Pictures
# if [ -f ~/Pictures/background.png ]; then
#     read -r -p "Overwrite background.png in ~/Pictures? [y/n] " input
#     case $input in
# 	[yY][eE][sS]|[yY])
# 	    cp ./Pictures/background.png ~/Pictures/background.png
# 	    ;;
# 	[nN][oO]|[nN])
# 	    echo "Skipping"
# 	    ;;
# 	*)
# 	    echo "Invalid input, exiting..."
# 	    exit 1
# 	    ;;
#     esac
# else
#     cp ./Pictures/background.png ~/Pictures/background.png
# fi
# TODO emacs structure
if [ -d ~/.emacs.d ]; then
    read -r -p "emacsdir exists, move to a backup? [y(es)/n(0)/s(kip)] " input
    case $input in
	[yY][eE][sS]|[yY])
	    mv ~/.emacs.d ~/.emacsbak
	    ;;
	[nN][oO]|[nN])
	    echo "deleting it and linking to dotfiles emacsdir"
	    rm -r ~/.emacs.d
	    ;;
	[sS])
	    echo "skipping copying"
	    ;;
	*)
	    echo "Invalid input, exiting..."
	    exit 1
	    ;;
    esac
fi

if [ "$input" != "s" ] || [ "$input" != "S" ]; then
    ln -s $dir/.emacs.d ~/.emacs.d
fi

# # TODO Copy emacsenv and eprofiles
echo "I assume you do not have anything in the directories: ~/.emacsenv ~/eprofiles"
ln -s $dir/.emacsenv ~/.emacsenv
ln -s $dir/eprofiles ~/eprofiles

echo "I will probably overwrite your existing chemacs profile files"
rm ~/.emacs-profile ~/.emacs-profiles.el
ln -s $dir/.emacs-profile ~/.emacs-profile
ln -s $dir/.emacs-profiles.el ~/.emacs-profiles.el

# TODO config structure
