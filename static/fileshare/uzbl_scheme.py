#!/usr/bin/env python
#
# url handler for uzbl
#
###

import os, subprocess, sys, urlparse

def detach_open(cmd):
    # Thanks to the vast knowledge of Laurence Withers (lwithers) and this message:
    # http://mail.python.org/pipermail/python-list/2006-November/587523.html
    if not os.fork():
        null = os.open(os.devnull,os.O_WRONLY)
        for i in range(3): os.dup2(null,i)
        os.close(null)
        subprocess.Popen(cmd)
    print 'USED'

# add various scheme to action statements here
if __name__ == '__main__':
    uri = sys.argv[8]
    u = urlparse.urlparse(uri)
    if u.scheme == 'mailto':
        detach_open(['urxvtc', '-e', 'mutt %s' % u.path])
    elif u.scheme == 'xmpp':
        detach_open(['gajim-remote', 'open_chat', uri])
    elif u.scheme == 'git':
        detach_open(['git', 'clone', uri], cwd=os.path.expanduser('~/'))
