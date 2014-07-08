# better-jump

`ace-jump-mode` done right.

Still very alpha, but usable.  Read the docstring of `bjump-jump` to learn how to simply create your own jumpers.

Currently provides these example jumps:

    bjump-char-jump-line (char)
    bjump-word-jump (head-char)
    bjump-word-jump-line (head-char)
    bjump-word-jump-paragraph (head-char)
    bjump-window-jump ()
    bjump-window-delete ()
    bjump-help-link-jump ()
    bjump-info-link-jump ()
    bjump-dired-jump ()

# Known issues

* If you display the same buffer in two windows and the visible portions overlap, the hints will go crazy.  This is almost never an issue for me so it is not high priority fix.  If anyone wants to fix this, I'll be very happy.
* Sometimes the visible buffer portion jumps up or down in very small windows.  This is some sort of "clever" Emacs hackery.  I can't quite figure out what is going on.
