# Orbmacs

Orbmacs is my relatively simple emacs init file, so called because of the dashboard image. It is my personal evolution of Witchmacs, and will be continually updated with changes I make.
I would encourage you to create your own fork of this repository, as not all changes may be to your liking.

# Installation
Run in a terminal
`git clone https://github.com/kvvba/orbmacs.git ~/.emacs.d`

Either restart emacs at this point or simply use `M-x load-file`, and point it to `~/.emacs.d/init.el`

All done!

## Optional

## Org 9.5

Check your org version with `M-x org-version`.
The built-in version of org (currently 9.4.4) forgoes some nice features.
Launch emacs from terminal with `emacs -q`, optionally with `-nw` to launch without a window system.
In emacs, do `M-x list-packages` to bring up a list of packages available.
Search for org using I-search, `C-s`. Press `C-s` to step through the search results until you reach org 9.5.x
Place the cursor over "org" and hit `RET`.
Hit `C-x o` to switch to the new window, and hit `RET` over the install button - or click on the install button.

# Notes

Default folders (you need to create these yourself)
- Org agenda: `~/org`
- Org journal: `~/org/journal`
- Org roam: `~/RoamNotes`