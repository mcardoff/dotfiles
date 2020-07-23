#!/bin/sh

get_volume() {
    stuff=$(amixer get Master | grep -oP '[^\[ ]*%')   
    echo $stuff
}

vol=$(get_volume | grep -oP '100|[0-9][0-9]')
num=$(( vol / 10 ))

case $num in
  0)  bar='----------' ;;
  1)  bar='|---------' ;;
  2)  bar='||--------' ;;
  3)  bar='|||-------' ;;
  4)  bar='||||------' ;;
  5)  bar='|||||-----' ;;
  6)  bar='||||||----' ;;
  7)  bar='|||||||---' ;;
  8)  bar='||||||||--' ;;
  9)  bar='|||||||||-' ;;
  10) bar='||||||||||' ;;
  *)  bar='----!!----' ;;
esac

echo $bar

exit 0
