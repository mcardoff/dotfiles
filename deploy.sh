#!/bin/bash

dir=$(pwd)

# This is meant to deploy the dotfiles so that I can simply clone the repo and get it done

[ "$(pwd | tail --bytes=9)" != "dotfiles" ] &&
    echo "Run this script in the repository which it originates from" &&
    exit 1

# DONE Copy wallpaper into ~/Pictures
if [ -f ~/Pictures/background.png ]; then
    printf '%s' "Overwrite background.png in ~/Pictures? [y/n] "
    read -r input
    case $input in
	[yY][eE][sS]|[yY])
	    cp ./Pictures/background.png ~/Pictures/background.png
	    ;;
	[nN][oO]|[nN])
	    echo "Skipping"
	    ;;
	*)
	    echo "Invalid input, exiting..."
	    exit 1
	    ;;
    esac
else
    cp ./Pictures/background.png ~/Pictures/background.png
fi

# shell and env vars
echo 'Sourcing environment variables'
# shellcheck source=~/.zshenv
. "$dir"/.zshenv

# handle scripts
echo "Linking $dir/.bin to $HOME/.local/scripts"
ln -s "$dir/.bin" "$HOME/.local/scripts"

# config directories
for subd in "$dir"/.config/*/ ; do
    progn=${subd::-1}
    progn=${progn/"$dir/.config/"} # program name
    
    echo "Backing up existing $XDG_CONFIG_HOME/$progn, replacing with my own"
    [ -d "$XDG_CONFIG_HOME/$progn" ] &&
     	mv "$XDG_CONFIG_HOME/$subd" "$XDG_CONFIG_HOME/$subd".old
	# echo "Moving $XDG_CONFIG_HOME/$progn to $XDG_CONFIG_HOME/$progn.old"

    echo "Linking $subd to $XDG_CONFIG_HOME/$progn"
    ln -s "$subd" "$XDG_CONFIG_HOME/$progn"
done

## Cleanup
echo 'Look at the following link for cleaning up your home directory:'
echo 'https://wiki.archlinux.org/title/XDG_Base_Directory'
