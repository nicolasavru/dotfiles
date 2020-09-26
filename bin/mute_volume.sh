#!/bin/bash

for SINK in `pacmd list-sinks | grep 'index:' | cut -b12-`
do
  # alternatively: @DEFAULT_SINK@
  pactl set-sink-mute $SINK toggle
done
