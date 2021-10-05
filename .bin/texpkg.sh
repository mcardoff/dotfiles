#!/bin/sh

# Directory to place it is
# Name of directory should be first argument, run with sudo
DIR=/usr/share/texmf/tex/latex/$1

if [ ! -d $DIR ]; then
    sudo mkdir $DIR
fi

if [ ! -d /home/mcard/Downloads/$1 ]; then
    unzip /home/mcard/Downloads/$1 -d /home/mcard/Downloads/
fi

cd /home/mcard/Downloads/$1

if [ ! -f /home/mcard/Downloads/$1/$1.sty ]; then
    if [ -f /home/mcard/Downloads/$1/$1.ins ]; then
	# we have to compile
	latex /home/mcard/Downloads/$1/$1.ins
    elif [ -f /home/mcard/Downloads/$1/$1.dtx ]; then
	tex /home/mcard/Downloads/$1/$1.dtx
    fi
fi

# now name.sty exists, copy it to texpath
sudo cp /home/mcard/Downloads/$1/$1.sty $DIR

# redo texhash
sudo texhash


