#!/bin/sh

AREA=$(slurp)

if [ -z "$AREA" ]; then
    exit 1
fi

exec grim -g "$AREA" - | swappy -f -
