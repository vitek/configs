#!/bin/sh

STATE=~/.config/brightnessctl.state
BRIGHTNESSCTL_OPTS="-q"

brightnessctl_save() {
    brightnessctl $BRIGHTNESSCTL_OPTS get > "$STATE"
}

brightnessctl_restore() {
    if [ -e "$STATE" ]; then
        brightnessctl $BRIGHTNESSCTL_OPTS set $(cat "$STATE")
    fi
}

case "$1" in
    up)
        brightnessctl $BRIGHTNESSCTL_OPTS set +10%
        brightnessctl_save
        ;;
    down)
        brightnessctl $BRIGHTNESSCTL_OPTS set 10%-
        brightnessctl_save
        ;;
    save)
        brightnessctl_save
        ;;
    restore)
        brightnessctl_restore
        ;;
    *)
        echo "Usage: $0 <up|down|save|restore>"
        ;;
esac
