#!/bin/sh

# NOTE: IntelliJ is usually used on HDMI1.  If it is not the primary monitor
# we have issues with the autocompletion box displaying on eDP1.

xrandr \
  --output eDP1 --mode 1680x1050 \
  --output HDMI1 --primary --mode 1920x1080 --above eDP1 \
  --output HDMI2 --mode 1920x1080 --right-of HDMI1
