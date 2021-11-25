#!/bin/sh

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

# TODO emacs structure
if [ -d ~/.emacs.d ]; then
    printf '%s' "Default emacs dir exists, move to a backup? [y(es)/n(o)/s(kip)] "
    read -r input
    case $input in
	[yY][eE][sS]|[yY])
	    mv ~/.emacs.d ~/.emacs.old
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

[ "$(echo "$input" | tr '[:upper:]' '[:lower:]')" != "s" ] &&
    ln -s "$dir/.emacs.d" ~/.emacs.d

# DONE Copy emacsenv and eprofiles
echo "I assume you do not have anything in the directories: ~/.emacsenv ~/eprofiles"
[ -d ~/.emacsenv ] && mv ~/.emacsenv ~/.emacsenv.old
ln -s "$dir"/.emacsenv ~/.emacsenv

[ -d ~/eprofiles ] && mv ~/eprofiles ~/eprofiles.old
ln -s "$dir"/eprofiles ~/eprofiles

echo "Backing up your existing chemacs profiles"
[ -f ~/.emacs-profile ] && mv ~/.emacs-profile ~/.emacs-profile.old
ln -s "$dir"/.emacs-profile ~/.emacs-profile

[ -f ~/.emacs-profiles.el ] && mv ~/.emacs-profiles.el ~/.emacs-profiles.el.old
ln -s "$dir"/.emacs-profiles.el ~/.emacs-profiles.el

# shell
echo 'Sourcing environment variables'
# shellcheck source=~/.zshenv
. "$dir"/.zshenv

# TODO config structure
## alacritty
echo 'backing up existing alacritty config, replacing with mine'
[ -d "$XDG_CONFIG_HOME"/alacritty ] &&
    mv "$XDG_CONFIG_HOME"/alacritty "$XDG_CONFIG_HOME"/alacritty.old

ln -s "$dir"/.config/alacritty "$XDG_CONFIG_HOME"/alacritty

## bashtop
echo 'backing up existing bashtop config, replacing with mine'
[ -d "$XDG_CONFIG_HOME"/bashtop ] &&
    mv "$XDG_CONFIG_HOME"/bashtop "$XDG_CONFIG_HOME"/bashtop.old

ln -s "$dir"/.config/bashtop "$XDG_CONFIG_HOME"/bashtop

## git
echo 'Attempting to copy your git config to config dir'
mkdir -p "$XDG_CONFIG_HOME"/git
if [ -f "$HOME/.gitconfig" ] ; then
    echo 'Found one!'
    mv "$HOME/.gitconfig" "$XDG_CONFIG_HOME"/git/config
else
    echo 'Did not find one, copying mine'
    mkdir -p "$XDG_CONFIG_HOME"/git
    ln -s "$dir"/.config/git "$XDG_CONFIG_HOME"/git/config
fi

## i3
echo 'backing up existing i3 config, replacing it with mine'
[ -d "$XDG_CONFIG_HOME"/i3 ] &&
    mv "$XDG_CONFIG_HOME"/i3 "$XDG_CONFIG_HOME"/i3.old

ln -s "$dir"/.config/i3 "$XDG_CONFIG_HOME"/i3

## kak
echo 'backing up existing kakoune config, replacing it with mine'
[ -d "$XDG_CONFIG_HOME"/kak ] &&
    mv "$XDG_CONFIG_HOME"/kak "$XDG_CONFIG_HOME"/kak.old

ln -s "$dir"/.config/kak "$XDG_CONFIG_HOME"/kak

## polybar
echo 'backing up existing polybar config, replacing it with mine'
[ -d "$XDG_CONFIG_HOME"/polybar ] &&
    mv "$XDG_CONFIG_HOME"/polybar "$XDG_CONFIG_HOME"/polybar.old

ln -s "$dir"/.config/polybar "$XDG_CONFIG_HOME"/polybar

## ranger
echo 'backing up existing ranger config, replacing it with mine'
[ -d "$XDG_CONFIG_HOME"/ranger ] &&
    mv "$XDG_CONFIG_HOME"/ranger "$XDG_CONFIG_HOME"/ranger.old

ln -s "$dir"/.config/ranger "$XDG_CONFIG_HOME"/ranger

## xmonad
echo 'Making proper directories for xmonad XDG home config'
mkdir -p "$XMONAD_CACHE_DIR"
mkdir -p "$XMONAD_CONFIG_DIR"
mkdir -p "$XMONAD_DATA_DIR"

ln -s "$dir"/.config/xmonad "$XDG_CONFIG_HOME"/xmonad

## zsh
echo 'checking for existing zsh files, moving to XDG config'
mkdir -p "$XDG_CONFIG_HOME"/zsh

if [ -f "$HOME/".zshrc ] ; then
    mv "$HOME"/.zshrc "$XDG_CONFIG_HOME"/zsh/.zshrc
else
    ln -s "$dir"/.config/zsh "$XDG_CONFIG_HOME"/zsh
fi

## Cleanup
echo 'Look at the following link for cleaning up your home directory:'
echo 'https://wiki.archlinux.org/title/XDG_Base_Directory'
