# Key repeat, 300 ms delay, 60 cps
xset r rate 300 60
# Allow horizontal scrolling
synclient HorizTwoFingerScroll=1
# Prevent tap to click
synclient MaxTapTime=0
# Set default screen brightness
xbacklight -set 50
# Start xbindkeys
xbindkeys
# Prevent clicking while typing
# syndaemon -i 0.4 -KRd
# Hack to get around missing keys
xmodmap -e 'keycode 66 = Caps_Lock'
# Load our actual xmodmap config
xmodmap $HOME/.Xmodmap
