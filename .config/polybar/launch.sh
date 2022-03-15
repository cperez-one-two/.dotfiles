#!/bin/sh

killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 0.5; done

outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)

if [ "$(hostname)" == "meep" ]; then
    tray_output=LVDS1
    mount='/home'
else
    tray_output=eDP-1
    mount='/'
fi


for m in $outputs; do
    case $m in
	*HDMI*)
	    tray_output=$m
	    ;;
    esac
done

for m in $outputs ; do
    if [[ $m == $tray_output ]]; then
	TRAY_POSITION=right MONITOR=$m MOUNTPOINT=$mount polybar --reload cwmbar 2>~/.cache/polybar/$m.log &
    else
	TRAY_POSITION=none MONITOR=$m MOUNTPOINT=$mount polybar --reload cwmbar 2>~/.cache/polybar/$m.log &
    fi
done
#polybar --reload example 2>~/.cache/polybar/$m.log &
