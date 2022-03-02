#!/bin/sh

xrandr --newmode "2560x1080_42" 155.50 2560 2688 2952 3344 1080 1083 1093 1109 -hsync +vsync

xrandr --addmode "HDMI2" 2560x1080_42

xrandr --output LVDS1 --auto --pos 1920x1080 --output HDMI2 --mode 2560x1080_42 --pos 0x0
