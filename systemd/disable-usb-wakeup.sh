#!/bin/sh

DEVICES="XHCI"
ACPI_WAKEUP=/proc/acpi/wakeup

for dev in $DEVICES; do
    if cat $ACPI_WAKEUP |grep -q "^$dev	.*enabled"; then
        echo "Disabling wake-up for $dev"
        echo "$dev" > $ACPI_WAKEUP
    fi
done
exit 0
