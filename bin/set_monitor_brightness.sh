#!/bin/bash
# https://github.com/rockowitz/ddcutil/issues/63

sudo ddcutil setvcp 10 "$@" --bus 4 &
sudo ddcutil setvcp 10 "$@" --bus 5 &
sudo ddcutil setvcp 10 "$@" --bus 6 &
sudo ddcutil setvcp 10 "$@" --bus 7 &
