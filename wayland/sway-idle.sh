#!/bin/sh

LOCK_CMD=sway-lock.sh

exec swayidle "$@" -w \
         lock         $LOCK_CMD  \
         before-sleep $LOCK_CMD  \
         timeout 5    "pgrep swaylock -U $UID && sway-dpms off" \
         resume       "pgrep swaylock -U $UID && sway-dpms on"  \
         timeout 120  $LOCK_CMD  \
         timeout 125  "sway-dpms off" \
         resume       "sway-dpms on"  \
