#!/bin/sh

essid=`iwconfig wlp2s0 | awk -F '"' '/ESSID/ {print $2}'`
echo $essid
