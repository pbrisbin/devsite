#!/bin/bash

if [ -f $HOME/.dmenurc ]; then
  . $HOME/.dmenurc
else
  DMENU="dmenu -i"
fi

file=$XDG_DATA_HOME/uzbl/history

# unique urls, most recent first
goto=`tac $file | awk '{print $3}' | uniq | grep -v ^$ | $DMENU`

[ -n "$goto" ] && echo "uri $goto" | socat - unix-connect:$5
