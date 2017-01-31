#### # Key repeat, 300 ms delay, 60 cps
xset r rate 300 60

# Default screen brightness
gmux_backlight 160

# Default keyboard brightness
xleds -set 10

xbindkeys

# Hack to get around missing keys
xmodmap -e 'keycode 66 = Caps_Lock'
# Load our actual xmodmap config
xmodmap $HOME/.Xmodmap

stalonetray -c ~/.xmonad/stalonetrayrc &
nm-applet &

gnome-screensaver &

# Start xmonad with `startx` from a ttl
exec xmonad
