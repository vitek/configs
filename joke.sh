#!/bin/sh

for dpy in `xrandr|awk '/^[a-zA-Z]/{print $1}'|tail -n +2`; do
    xrandr --output $dpy --rotate normal
done
