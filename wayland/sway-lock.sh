#!/bin/sh

SWAYLOCK_ARGS="-f -c 000000"

BG=$(desktopctl  bg-image 2> /dev/null)

if [ "x$BG" != "x" ]; then
    SWAYLOCK_ARGS="-i $BG"
fi

swaymsg 'input "*" xkb_switch_layout 0'

exec swaylock $SWAYLOCK_ARGS "$@"
