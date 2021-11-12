#!/bin/bash

[ -z "$(pgrep ranger)" ] && alacritty -t Ranger -e ranger		     
