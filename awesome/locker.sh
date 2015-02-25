#!/bin/sh


LOCK_COMMAND="i3lock -d -c 000000"
LOCK_TIMEOUT=10


case "$1" in
    autolocker)
        exec xautolock -detectsleep \
            -time $LOCK_TIMEOUT -locker "$LOCK_COMMAND" \
            -notify 5 \
            -notifier "notify-send -u critical -t 5000 -- 'LOCKING screen in 5 seconds'"
        ;;
    lock)
        exec xautolock -locknow
        ;;
esac

