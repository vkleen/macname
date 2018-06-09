#!/usr/bin/env bash

[[ -z "$1" ]] && exit 1
target="$1"

while true; do
    uuid=$(dbus-uuidgen)
    trial=$(./result/bin/macname "${uuid}" | awk '{print $2;}')
    if (( trial == target )); then
        ./result/bin/macname "${uuid}"
        exit 0
    fi
done
