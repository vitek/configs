#!/bin/sh

swaymsg  'input "*" xkb_switch_layout 0'
exec swaylock "$@"
