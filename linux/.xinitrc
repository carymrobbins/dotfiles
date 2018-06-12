# Load resources
[ -f $HOME/.Xresources ] && xrdb -merge -I$HOME $HOME/.Xresources

# Default screen brightness
xleds screen -set 5%

# Load our x settings (xset, xkb, xinput, etc.)
reload-x-settings

# TODO: wm.target doesn't seem to work
# systemctl --user start wm.target
systemctl --user start gnome-screensaver
systemctl --user start blueman-applet
# disabling in favor of polybar
#systemctl --user start stalonetray
systemctl --user start nm-applet
systemctl --user start notify-osd
systemctl --user start low-battery.timer
systemctl --user start xcompmgr
systemctl --user start redshift
systemctl --user start polybar
# TODO: Running xbindkeys with systemctl means it
# can't find stuff in our $PATH
xbindkeys

if command -v mon1 >/dev/null; then
  mon1
fi

# This is needed for java apps to work properly within xmonad
# e.g. visualvm, yourkit
export _JAVA_AWT_WM_NONREPARENTING=1

xmonad --recompile

# Start xmonad with `startx` from a tty
exec xmonad
