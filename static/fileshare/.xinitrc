#!/bin/sh

eval `cat $HOME/.fehbg` &                   # set background
numlockx on &                               # numlock on
setxkbmap -option terminate:ctrl_alt_bksp & # C-A-BS to kill X
urxvtd -q -f -o &                           # start urxvt daemon
xcompmgr -fF &                              # allow transparency
xmodmap $HOME/.Xmodmap &                    # remove capslock
xrdb $HOME/.Xdefaults &                     # load Xdefaults
xset fp+ /usr/share/fonts/local &           # add local fonts
xset s 1200 1200 &                          # set monitor to sleep 20 mins
xsetroot -cursor_name left_ptr              # req'd for xmonad's cursor
xmonad                                      # start xmonad
