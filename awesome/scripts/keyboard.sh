#!/bin/sh

setxkbmap -layout "us,ru"
kbdd

# See https://github.com/awesomeWM/awesome/issues/196
xkbcomp $DISPLAY - | egrep -v "group . = AltGr;" | xkbcomp - $DISPLAY
