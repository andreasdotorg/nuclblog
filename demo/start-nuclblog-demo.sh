#!/bin/sh
detachtty --dribble-file log/nuclblog-demo.dribble \
    --log-file log/detachtty.log \
    --pid-file log/nuclblog-demo.pid \
    log/nuclblog-demo.socket \
    /usr/local/bin/sbcl \
    --eval '(load "start-nuclblog-demo.lisp")'
