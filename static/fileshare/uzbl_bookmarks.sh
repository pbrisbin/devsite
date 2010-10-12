#!/bin/bash
#
# PREAMBLE {{{
#
# pbrisbin 2009
#
# http://pbrisbin.com/bin/uzbl_bookmarks.sh
#
# create/load an html page of # links based on a folder
# hierchy of bookmark lists
#
# keep $bookmarks organized something like this, where the 
# ONLY info in the individual bookmark text files is listed 
# urls of your bookmarked links corresponding to the
# containing dir's category
# 
# $bookmarks
# |-- linux
# |   |-- arch
# |   |   `-- bookmarks
# |   |-- cli
# |   |   `-- bookmarks
# |   |-- howto
# |   |   `-- bookmarks
# |   |-- mutt
# |   |   `-- bookmarks
# `-- mac
#     |-- howto
#     |   `-- bookmarks
#     |-- programs
#     |   `-- bookmarks
#     `-- reference
#         `-- bookmarks
# 
# spaces in filenomes not entirely supported
#
# this script was orginally designed for use with uzbl
# is /should/ still work that way, but i've tried to
# generalize it and might have inadvertantly broken
# something
#
# }}}
###

# help message
message() {
  echo 'write a damn help message'
  echo
  
  exit 1
}

# create the html page of all your bookmarks
create_page() {
  local LC_ALL='C'

### HTML HEADER {{{
#
# this html header defines a simple page titled "My Bookmarks" with
# a light grey background, bright white headings, green unvisited links
# and dark grey links
#
###

  cat > $tmp_page << EOF
<html>
<head>
  <title>My Bookmarks</title>
  <style media="screen" type="text/css">
  body {
    margin-left: 10%; margin-right: 10%;

    color: #bbbbbb; 
    background: #303030;
    background-color: #303030;
    bgcolor: #303030;

    font-family: Lucida Sans, Verdana, sans-serif; 
    font-weight: normal;
  }

  p {
    color: #bbbbbb;
    background-color: #303030;
  }

  h1 {
    text-align: center;
    color: #ffffff;
    font-family: Lucida Sans, Verdana, sans-serif;
    font-weight: normal;
  }

  h2,h3,h4,h5,h6 {
    color: #ffffff;
    margin-left: -5%;
    font-family: Lucida Sans, Verdana, sans-serif;
    font-weight: normal;
  }

  a:link {
    color: #c4df90 !important;
    text-decoration: none !important;
    outline: none;
  }

  a:active {
    color: #cc896d !important;
    text-decoration: none !important;
    outline: none;
  }

  a:hover {
    color: #cc896d !important;
    text-decoration: none !important;
    outline: none;
  }

  a:visited {
    color: #909090 !important;
    text-decoration: none !important;
    outline: none;
  }
  </style>
</head>
    
<body>
<h1>Bookmarks</h1>
<hr />

EOF
# }}}

  # here we search the folder structure and create the body
  # of the page
  for path in $(find "$bookmarks" -type f | sort); do
    folder="$(dirname "$path")"

    echo "<h2>${folder/$bookmarks/}</h2>" >> $tmp_page

    while read url; do
      echo "<p><a href=\"$url\">$url</a></p>" >> $tmp_page
    done < "$path"

  done

### HTML FOOTER {{{

  # write simple footer
  cat >> $tmp_page << EOF

<hr />
</body>

</html>
EOF

# }}}

}

# insert a bookmark from UZBL
insert_bookmark_uzbl() {
  echo "i would insert $1 as a url"
  return

  local LC_ALL="C"

  pushd "$bookmarks" &>/dev/null

  DIR="$( (echo NEW; find ./ -mindepth 1 -type d | sort) | zenity --list --title="Choose category" --column="Categories")"
  [ -z "$DIR" ] && exit 1

  if [ "$DIR" = "NEW" ]; then
    sleep 1 # sleep needed or zenity eats itself

    DIR="$(zenity --entry --entry-text="./new/category" --title="Enter new category")"
    [ -z "$DIR" ] && exit 1
  fi

  # create the category if it's new
  if [ ! -d "$DIR" ]; then
    mkdir -p "$DIR"
    touch "$DIR/bookmarks"
  fi

  # prevent dupes within the category
  grep -Fqx "$1" "$DIR/bookmarks" || echo "$1" >> "$DIR/bookmarks"

  popd &>/dev/null

  create_page
}

# load a bookmark in UZBL
load_from_bookmark_uzbl() {
  [ ! -f "$tmp_page" ] && create_page

  echo "i would run $UZBL -u file://$tmp_page"
  return

  $UZBL -u "file://$tmp_page" &>/dev/null &
}

# uzbl command
UZBL='uzbl-browser'

# directory under which all your bookmarks reside
bookmarks="$HOME/.my_bookmarks"

# store the rendered page here
tmp_page='/srv/http/shared/bookmarks.html'

args=( "$@" )

if [ "${args[7]}" = "insert" ]; then
  insert_bookmark_uzbl "${args[5]}"
else
  load_from_bookmark_uzbl
fi
