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
systemctl --user start tau.timer
systemctl --user start time-to-move.timer
systemctl --user start xcompmgr
systemctl --user start geoclue-redshift-agent
systemctl --user start redshift
systemctl --user start polybar
# TODO: Running xbindkeys with systemctl means it
# can't find stuff in our $PATH
xbindkeys

if command -v monx >/dev/null; then
  # Assume this is our hidpi monitor (for now)
  if xrandr | grep 'DP-1 connected' > /dev/null; then
    monx 0
  else
    monx 1
  fi
fi

# This is needed for java apps to work properly within xmonad
# e.g. visualvm, yourkit
export _JAVA_AWT_WM_NONREPARENTING=1

xmonad --recompile

# Start xmonad with `startx` from a tty
exec xmonad
