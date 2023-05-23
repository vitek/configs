#!/bin/sh

if [ "$1" = "off" ]; then
    exec swaymsg 'output * dpms off'
else
    exec swaymsg 'output * dpms on'
fi
