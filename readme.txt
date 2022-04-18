The purpose of evics is to be a simple vim emulation package that
tries to make use of as much of Emac's internal functions as
possible. The tradeoff is that it does not contain all of vim's
functionality, and that there are differences in behaviours between
the emulated commands and the native vim one's.

A very rudimentary "command" mode was implemented, for example, to
open a file:

   :e <ret>

In normal vim, you would simple do:

   :e <filename>

As of now, all of the commands in "command" mode operate like this,
the underlying command is only invoked after you hit return.

To view the implemented keybindings, please refer to evics-normal.el.

Its important to note the existence of
evics-visual-transient-mode-map. This map is activated with
set-transient-map whenever the mark is activated. A use for this map
would be to put some user defined functions to enhance text
selection. For example, adding a keybinding that would select the
whole active function.


This package is not complete, it is still in it's early
stages. Originally I was using the evil package, but I was annoyed how
it took over everything and felt the need to re-implement a lot of
functionality that already existed inside of Emacs. This package is
mainly for my personal use, I have put it online in case someone else
finds use in it. To use, simply clone this git repo and use the
following elisp snippet:

     (add-to-list 'load-path "<PATH TO EVICS>")
     (require 'evics)
     (evics-global-mode t)

You will most likely want to install undo-tree. Atleast until evics is
reworked to use emacs 28's built in undo-redo.
