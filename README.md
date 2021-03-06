What is it?
===========
An extremely simple sprite editor in haskell, using the Gloss library.

This is just a learning exercise by a haskell beginner.

![screenshot](https://github.com/nbogie/sprite-editor-haskell/raw/master/docs/sprite-editor-screenshot-1.png)

Keys:
=====

Sprite Editing functions
------------------------

 * Cursor Keys: move cursor in sprite-editing canvas
 * '1' - '9' color pixel under cursor (observing symmetry rules)
 * '0' - erase pixel under cursor (observing symmetry rules)
 * 'a' toggle axis of symmetry [vert,horiz,both,none]
 * 'm' mirror current sprite
 * 'r' rotate current sprite
 * 'h' apply a drop shadow to the pixels of the current sprite which have the same color as the current pixel. (Wipes previous shadow)
 * 'u' undo (infinite)
 * 'w' erase (wipe) sprite
 * 'z' erase color under pixel
 * 's' save current sprite to library (in-memory only)
 * Return: Start / Stop renaming current sprite. Hit return, type a name, and hit return to finish.

Library functions
------------------------

 * Shift-Cursor Keys: Move library cursor (allows for load, save, rename of sprite)
 * 'l' load sprite at library cursor into editing canvas
 * 'X' delete sprite at library cursor
 * 'S' save sprite library to file (sprites.dat).

Board functions
------------------------

The "Board" has not been given much attention.
 * Ctrl-Cursor Keys: Move board cursor
 * 'p' place current sprite on the board at cursor position.

