#!/bin/bash
#
# pbrisbin 2009
#
###

uzbl_sok="$5" # our socket
curr_url="$6" # our url

history_file="$XDG_DATA_HOME/uzbl/history" # our history file

[ ! -f "$history_file" ] && exit 1

# print the history file in reverse, strip to just urls
tac "$history_file" | awk '{print $3}' | while read this_url; do
  # strip to just the domain
  curr_domain=$(echo $curr_url | cut -d '/' -f 3)
  this_domain=$(echo $this_url | cut -d '/' -f 3)

  # find the next entry from a different domain
  if [ "$this_domain" != "$curr_domain" ]; then
    echo uri $this_url | socat - unix-connect:$uzbl_sok &
    break
  fi
done
