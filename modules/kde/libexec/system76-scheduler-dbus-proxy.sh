#!/bin/bash
#
# Currently, system76-scheduler D-Bus service listens on the system bus, but there's no way for
# a KWin script to send messages to the system bus. This script is a workaround that listens on
# the session bus for messages and forwards them to the system bus.

DBUS_SERVICE="com.system76.Scheduler"
DBUS_PATH="/com/system76/Scheduler"
DBUS_INTERFACE="com.system76.Scheduler"
DBUS_METHOD="SetForegroundProcess"

dbus-monitor --session "destination=$DBUS_SERVICE,path=$DBUS_PATH,interface=$DBUS_INTERFACE,member=$DBUS_METHOD" |
	while true; do
		read -r method_call
		read -r _ pid

		if [[ $method_call =~ ^method\ call.*member=$DBUS_METHOD ]]; then
			dbus-send --system --print-reply \
				--dest=$DBUS_SERVICE $DBUS_PATH $DBUS_INTERFACE.$DBUS_METHOD "uint32:$pid" >/dev/null
		fi

		sleep .001	# Prevent busy loop if something fails...
	done
