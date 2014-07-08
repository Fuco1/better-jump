# better-jump

`ace-jump-mode` done right.

Still very alpha, but usable.

The goal of this package is to provide a simple way to define "jumpers" which could execute arbitrary actions at places by picking the destination from a selection of hints (like `ace-jump-mode`, the conkeror web browser and others).

This package provides a single function `bjump-jump` which is a "skeleton", responsible for setting up the hints, picking the correct one, limiting the scope of the actions or selections and so on.  It takes other functions as arguments, where each of those is responsible for a certain part of the process: finding the selection candidates, displaying the hints, letting the user pick the target, limiting the scope where we look for the candidates (which windows and which frames to consider, what portions of the currently visible windows: current line, paragraph, entire page, next 3 words...).  This makes it extremely flexible and extendable---you can reuse many of the parts and simply modify their behaviour by adding or removing various "filters".

Read the docstring of `bjump-jump` to learn how to simply create your own jumpers.  Look at the definitions of the built-in jumpers if you need inspiration, most of them are under 10 lines long.

There is also great many of predefined selectors, window and frame scope resolution functions, hint pickers, actions and action combinators, so you can simply pick what you want and "LEGO" your own jumpers.  With the built-in functions there's already way over 1000 possible combinations!

# Built-in jumpers

We currently provide these example jumps:

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
