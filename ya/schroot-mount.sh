#!/bin/sh


SCHROOT=/run/schroot/mount/${1}/

mount --rbind /home $SCHROOT/home
mount --make-rslave $SCHROOT/home

mount --rbind /run/user $SCHROOT/run/user
mount --make-rslave $SCHROOT/run/user
