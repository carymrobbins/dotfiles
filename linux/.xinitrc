# https://wiki.archlinux.org/title/NVIDIA_Optimus#Available_methods
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

# Run with `NOSCALE=1 startx` to disable hidpi scaling

if [ -z "$NOSCALE" ]; then
  # Load resources
  [ -f $HOME/.Xresources ] && xrdb -merge -I$HOME $HOME/.Xresources
  # Some hacks to make apps like unity render better in hidpi
  export GDK_SCALE=2
  export GDK_DPI_SCALE=0.5
fi

# Default screen brightness
xleds screen -set 5%

# Load our x settings (xset, xkb, xinput, etc.)
reload-x-settings
xkb-reload

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
systemctl --user start xbindkeys

if command -v monx >/dev/null; then
  if [ -z "$NOSCALE" ]; then
    monx auto
  else
    monx noscale
  fi
fi

# This is needed for java apps to work properly within xmonad
# e.g. visualvm, yourkit
export _JAVA_AWT_WM_NONREPARENTING=1

# Start xmonad with `startx` from a tty
exec xmonad
