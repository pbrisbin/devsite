#!/bin/bash
# 
# pbrisbin 2009
#
# http://pbrisbin.com/bin/uzbl_download.sh
#
# zenity download script
# modified from iosonofabio's
#
#########################################################

# download link passed in from uzbl
URL="$8"

# the place to download files to
DIR="$HOME/Downloads"

# get filename from the url and convert some hex codes
# i hate spaces in filenames so i'm switching them
# with underscores here, adjust the first s///g if
# you want to keep the spaces. i'm also switching
# to all lowercase.
FILE="$(basename "$URL" | tr 'A-Z' 'a-z' | sed -r \
's/[_%]20/\_/g;s/[_%]22/\"/g;s/[_%]23/\#/g;s/[_%]24/\$/g;
s/[_%]25/\%/g;s/[_%]26/\&/g;s/[_%]28/\(/g;s/[_%]29/\)/g;
s/[_%]2C/\,/g;s/[_%]2D/\-/g;s/[_%]2E/\./g;s/[_%]2F/\//g;
s/[_%]3C/\</g;s/[_%]3D/\=/g;s/[_%]3E/\>/g;s/[_%]3F/\?/g;
s/[_%]40/\@/g;s/[_%]5B/\[/g;s/[_%]5C/\\/g;s/[_%]5D/\]/g;
s/[_%]5E/\^/g;s/[_%]5F/\_/g;s/[_%]60/\`/g;s/[_%]7B/\{/g;
s/[_%]7C/\|/g;s/[_%]7D/\}/g;s/[_%]7E/\~/g;s/[_%]2B/\+/g')"

# auto open the file post-download
open() {
  case "$1" in
    *.pdf)                                      epdfview "$1" &                    ;;
    *.jpg|*.png|*.jpeg)                         mirage "$1" &                      ;;
    *.mp3|*.aac|*.flac|*.ogg|*.m4a)             playnow "$1" &                     ;;
    *.txt|*README*|*.pl|*.sh|*.py|*.hs)         urxvtc -e bash -cl "vim \"$1\"" &  ;;
    *.mov|*.avi|*.mpeg|*.mpg|*.flv|*.wmv|*.mp4) mplayer "$1" &                     ;;
  esac
}

# download with zenity
wget -O "$DIR/$FILE" --user-agent=Firefox "$URL" 2>&1 \
| sed -u 's/^[a-zA-Z\-].*//; s/.* \{1,2\}\([0-9]\{1,3\}\)%.*/\1\n#Downloading... \1%/; s/^20[0-9][0-9].*/#Done./' \
| zenity --progress \
         --auto-kill \
         --auto-close \
         --percentage=0 \
         --title="Download of $FILE" \
         --text="$FILE"


# auto-open the downloaded file
[ $? -eq 0 ] && open "$DIR/$FILE"
