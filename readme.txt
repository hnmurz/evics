The purpose of evics is to be a simple vim emulation package that
tries to make use of as much of Emac's internal functions as
possible. The tradeoff is that it does not contain all of vim's
functionality, and that there are differences in behaviours between
the emulated commands and the native vim one's.

As some background, originally I was using the evil package, but I was
annoyed how it took over everything and felt the need to re-implement
a lot of functionality that already existed inside of Emacs.

This package is mainly for my personal use, I have put it online in
case someone else finds use in it. As of now, this package is
relatively complete. It serves most of my personal purposes.


A very rudimentary "command" mode was implemented, for example, to
open a file:

   :e <ret>

In normal vim, you would simple do:

   :e <filename>

As of now, all of the commands in "command" mode operate like this,
the underlying command is only invoked after you hit return.

To view the implemented keybindings, please refer to evics-normal.el.

Its important to note the existence of
evics-mark-active-mode-map. This map is activate whenever the mark is
activate. A use for this map would be to put some user defined
functions to enhance text selection. For example, adding a keybinding
that would select the whole active function:

   (define-key evics-mark-active-mode-map (kbd "f") 'mark-defun)


To use, simply clone this git repo and use the following elisp
snippet:

     (add-to-list 'load-path "<PATH TO EVICS>")
     (require 'evics)
     (evics-global-mode t)

You will most likely want to install undo-tree. Atleast until evics is
reworked to use emacs 28's built in undo-redo.
