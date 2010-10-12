#!/bin/bash
#
# pbrisbin 2009
#
# http://pbrisbin.com/bin/uzbl_session.sh
#
# * uzbl_session.sh
#
# * an alternate session script using sockets
#
# * requires socat
#
# add to ~/.config/uzbl/config
#
#   # close just this instance
#   @bind :c_  = exit
#
#   # close all instances and clear session file
#   @bind :q!_ = spawn /path/to/script.sh --quit
#
#   # close all instances and save session
#   @bind :wq_ = spawn /path/to/script.sh --save
#
#   the '_' just forces you to hit <enter>
#
###

# close all and optionally save session
close_session() {
  rm -rf $session_file

  #  all but current window
  for socket in $socket_dir/uzbl_socket_*; do
    [ $save -eq 1 ] && echo 'print @uri' | socat - unix-connect:$socket | grep -v ^EVENT >> $session_file
    echo 'exit' | socat - unix-connect:$socket >/dev/null
  done

  # added bonus cleanup time
  rm -rf "$socket_dir/uzbl_{fifo,socket}*"
}

# open uzbl (maybe use session_file)
open_session() {
  if [ -s $session_file ]; then
    while read url; do
      uzbl-browser -u "$url" $uzbl_opts &
    done < $session_file
  else
    uzbl-browser $uzbl_opts &
  fi
}

args=( "$@" )

if [ ${#args[@]} -ge 7 ]; then
  command=${args[7]}     # $8 is the argument to this script
  uzbl_opts=${args[@]:8} # $9.. are arguments for launched uzbls
else
  command=${args[0]}
  uzbl_opts=${args[@]:1}
fi
 
# constants
socket_dir="/tmp"
session_file="$HOME/.uzbl-session"

save=1

# run it
case $command in
  -q|--quit) save=0
             close_session ;;
  -s|--save) close_session ;;
  -n|--new)  open_session  ;;
  *)         exit 1        ;;
esac
