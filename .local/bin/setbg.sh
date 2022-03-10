#!/bin/sh

# Location of link to wallpaper link.
bgloc="${XDG_DATA_HOME:-$HOME/.local/share/}/background"

trueloc="$(readlink -f "$1")" &&
case "$(file --mime-type -b "$trueloc")" in
    image/*)
        ln -sf "$(readlink -f "$1")" "$bgloc" &&
            notify-send -i "$bgloc" "Changing wallpaper..."
        ;;
    inode/directory)
        ln -sf "$(find "$trueloc" -iregex '.*.\(jpg\|jpeg\|png\|gif\)' -type f | shuf -n 1)" "$bgloc" &&
            notify-send -i "$bgloc" "Random Wallpaper chosen."
        ;;
    *)
       notify-send "Error" "Not a valid image." ; exit 1
       ;;
esac

feh --bg-center "$bgloc"
