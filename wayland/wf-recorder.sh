#!/bin/sh

# https://github.com/ammen99/wf-recorder/wiki#using-wf-recorder-as-camera

find_wfrecord() {
    for sysfs_path in /sys/class/video4linux/video*; do
        if [ "$(cat $sysfs_path/name)" = "WfRecorder" ]; then
            echo /dev/$(basename $sysfs_path)
            return 0
        fi
    done
    return 1
}

die() {
    echo "$@" >&2
    exit 1
}

VIDEO=$(find_wfrecord)

if [ "$VIDEO" = "" ]; then
    die "No video device.

/etc/rc.local:
modprobe v4l2loopback exclusive_caps=1 card_label=WfRecorder
systemctl enable --now rc-local
"
fi

OUTPUT=$(swaymsg -t get_outputs -r |
             jq -r '.. | select(.focused?) | .name')

if [ "$OUTPUT" = "" ]; then
    die "No output found"
fi

echo "Video device: $VIDEO"
echo "Output: $OUTPUT"

exec wf-recorder --muxer=v4l2 --codec=rawvideo -x yuv420p \
     "--file=$VIDEO" \
     "--output=$OUTPUT"
