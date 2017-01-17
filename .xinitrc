#### # Key repeat, 300 ms delay, 60 cps
xset r rate 300 60

#### TODO: Backlight doesn't work yet
#### xbacklight -set 10

xleds -set 10

#### # Start xbindkeys
xbindkeys

# Hack to get around missing keys
xmodmap -e 'keycode 66 = Caps_Lock'
# Load our actual xmodmap config
xmodmap $HOME/.Xmodmap

# Start xmonad with `startx` from a ttl
exec xmonad
