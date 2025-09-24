#!/bin/sh
xrandr --output eDP-1 --mode 1680x1050 --pos 1126x1200 --rotate normal \
       --output DP-1 --off \
       --output HDMI-1 --off \
       --output HDMI-2 --off \
       --output DP-2 --off \
       --output DP-3 --off \
       --output DP-4 --off \
       --output DP-3-1 --primary --mode 1920x1200 --pos 1920x0 --rotate normal \
       --output DP-3-2 --off \
       --output DP-3-3 --mode 1920x1200 --pos 0x0 --rotate normal
