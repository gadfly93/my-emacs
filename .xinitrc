#!/bin/sh

# /etc/X11/xinit/xinitrc
#
# global xinitrc file, used by all X sessions started by xinit (startx)

# invoke global X session script
# . /etc/X11/Xsession

#!/usr/bin/env bash
#
# This is a sample file for the ~/.xinitrc file.
#

# You may need to comment out the next line to disable access control
#xhost +

# Set themes, etc
gnome-settings-daemon &

# # Set fallback cursor
# xsetroot -cursor_name left_ptr

# If Emacs is started in server mode, `emacsclient` is a convenient way to edit
# files in place (used by e.g. `git commit`)
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# # Disable access control
# xhost +SI:localuser:$USER

# ## Over-rides
# #
# xset b off &
# #numlockx off                    # Turn off numlock

# Set keyboard repeat rate
xset r rate 180 45

# Programs to start upon startup
xfce4-power-manager &
# xfce4-panel&                    # Systray
nm-applet &                       # Network Manager
# pasystray &                     # Pulseaudio volume control from tray
# udiskie --tray &                # Disk mount utility

# launch emacs upon startup
exec dbus-launch --exit-with-session emacs
