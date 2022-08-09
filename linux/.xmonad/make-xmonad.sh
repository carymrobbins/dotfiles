#!/bin/bash

set -ex

cd ~/.xmonad/xmonad-dbus
cabal install
stack install
sudo mv ~/.local/bin/xmonad-dbus /usr/bin/xmonad-dbus
xmonad --recompile
