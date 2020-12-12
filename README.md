# octree

This is a rough playground for making animated GIFs with vector
drawing functions from
[Vecto](https://www.xach.com/lisp/vecto/)/Vectometry.

To try it:

    $ (cd ~/quicklisp/local-projects && git clone https://github.com/xach/octree.git)

    > (ql:quickload "octree")
    > (in-package :octree)
    > (test-hex-overlap-animation "whee.gif")

whee.gif will look something like
https://twitter.com/xach/status/1336489161411407877

See text-art.lisp for some other fun text-oriented animation. Needs a
TrueType .ttf font file to work.
