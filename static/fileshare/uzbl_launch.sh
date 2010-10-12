#!/bin/bash
#
# all purpose url launcher for uzbl
#
# add to uzbl config:
#
#   @bind  :o_  spawn /path/to/this our %s
#   @bind  :n_  spawn /path/to/this new %s
#
# NOTE: doing it this way (:o_ vs :o _) lets us
#   hit :o<enter> to just go home and :n<enter> to
#   open a new window at our home page.  :o someurl
#   still works fine, but the mode is passed in as
#   $8, a space is passed as $9, and the url is
#   passed as $10 (shift; $9).  this is accounted
#   for in the script, but if you use a different
#   binding you'll need to adjust this part.
#
# so:
#
#   our %s means open %s in this window
#   new %s means open %s in a new window
#
###

# homepage, used when no url is passed
home_url="http://www.google.com"

### functions

# relative path -> absolute one
rel2abs() {
  local file="$(basename "$1")"
  local dir="$(dirname "$1")"

  pushd "${dir:-./}" &>/dev/null && local dir="$PWD"
  popd &>/dev/null

  echo "$dir/$file"
}

# check if a scheme was passed
has_scheme() {
  string="$URL"

  # contains '://' -> has scheme
  [ "$string" != "${string/\:\/\//}" ] && return 0

  return 1
}

# check if a file exists
is_a_file() {
  string="$(rel2abs "$URL")"

  # doesn't exist -> not a file
  if [ -e "$string" ]; then
    URL="$string"
    return 0
  fi

  return 1
}

# check that it's a search term
not_a_url() {
  string="$URL"
  
  # spaces -> not a url
  [ "$string" != "${string// /}" ] && return 0

  # no dots -> not a url
  [ "$string" != "${string//./}" ] || return 0

  return 1
}


### the script

# handle input
if [ $# -gt 2 ]; then # we're being called from uzbl
  shift 4; SOCK="$1"
  shift 3; MODE="$1"
  shift 2; URL="$@"
else # we're being called outside of uzbl
  MODE="new"
  URL="$*"
fi

# TODO: handle mailto:// with mutt right away

# no url -> use home
URL="${URL:-$home_url}"

# if a scheme is passed, use URL as is
if has_scheme; then
  URL="$URL"

# if a file exists, use file://URL
elif is_a_file; then
  URL="file://$URL"

# if URL contains spaces and/or no dots, google it
elif not_a_url; then
  URL="http://www.google.com/search?q=${URL// /+}"

# otherwise add http://
else
  URL="http://$URL"

fi

# open in new window or our window
case $MODE in
  our) echo "uri $URL" | socat - unix-connect:$SOCK & ;;
  new) uzbl-browser -u "$URL" &                       ;;
esac
