#### # Key repeat, 300 ms delay, 60 cps
xset r rate 300 60

# Default screen brightness
gmux_backlight 160

# Default keyboard brightness
xleds -set 10

# Hack to get around missing keys
xmodmap -e 'keycode 66 = Caps_Lock'
# Load our actual xmodmap config
xmodmap $HOME/.Xmodmap

# TODO: wm.target doesn't seem to work
# systemctl --user start wm.target
systemctl --user start gnome-screensaver
systemctl --user start stalonetray
systemctl --user start nm-applet
systemctl --user start notify-osd
systemctl --user start low-battery.timer
# TODO: Running xbindkeys with systemctl means it
# can't find stuff in our $HOME/bin
xbindkeys

if command -v mon1 >/dev/null; then
  mon1
fi

# Start xmonad with `startx` from a tty
exec xmonad
