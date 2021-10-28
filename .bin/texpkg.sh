#!/bin/bash

# Directory to place it is
# Name of directory should be first argument, run with sudo
DIR=/usr/share/texmf/tex/latex/
cd /home/mcard/Downloads/package_install

for z in *.zip; do
    # echo $z
    if [ ! -d $DIR/$z ]; then
	sudo mkdir $DIR/${z%.*}
    fi
    # unzip
    unzip $z
    if [ ! -f ./${z%.*}.sty ]; then # .sty not available, compile instead
	if [ -f ./${z%.*}/${z%.*}.ins ]; then
	    latex ./${z%.*}/${z%.*}.ins
	elif [ -f ./${z%.*}/${z%.*}.dtx ]; then
	    tex ./${z%.*}/${z%.*}.dtx
    fi
fi
done


# cd /home/mcard/Downloads/$1

# if [ ! -f /home/mcard/Downloads/$1/$1.sty ]; then
    # if [ -f /home/mcard/Downloads/$1/$1.ins ]; then
	# we have to compile
	# latex /home/mcard/Downloads/$1/$1.ins
    # elif [ -f /home/mcard/Downloads/$1/$1.dtx ]; then
	# tex /home/mcard/Downloads/$1/$1.dtx
    # fi
# fi

# # now name.sty exists, copy it to texpath
# sudo cp /home/mcard/Downloads/$1/$1.sty $DIR

# # redo texhash
# sudo texhash


