#!/bin/sh

killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 0.5; done

outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)
tray_output=eDP-1


for m in $outputs; do
	if [[ $m == "HDMI-2" ]]; then
		tray_output=$m
	fi
done

for m in $outputs ; do
	if [[ $m == $tray_output ]]; then
		TRAY_POSITION=right MONITOR=$m polybar --reload cwmbar 2>~/.cache/polybar/$m.log &
	else
		TRAY_POSITION=none MONITOR=$m polybar --reload cwmbar 2>~/.cache/polybar/$m.log &
	fi
done
#polybar --reload example 2>~/.cache/polybar/$m.log &
