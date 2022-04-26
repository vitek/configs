#!/bin/sh

set -ex

. "$SETUP_DATA_DIR/common-data"
. "$SETUP_DATA_DIR/common-functions"
. "$SETUP_DATA_DIR/common-config"

if [ $STAGE = "setup-start" ] || [ $STAGE = "setup-recover" ]; then
    mkdir -p "$CHROOT_PATH/home" "$CHROOT_PATH/run/user"

    mount --rbind /home "$CHROOT_PATH/home"
    mount --make-rslave "$CHROOT_PATH/home"

    mount --rbind /run/user "$CHROOT_PATH/run/user"
    mount --make-rslave "$CHROOT_PATH/run/user"
fi
