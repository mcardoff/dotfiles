#!/bin/sh
isconnected=$(lsusb | grep "ThinkPad Dock Hub" | head -n 1)
[[ -z "$isconnected" ]] && echo "Not connected"
# echo $isconnected
