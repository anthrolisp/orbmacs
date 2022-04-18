# Orbmacs

![O R B](https://github.com/kvvba/orbmacs/blob/ed9e8d190ea64375a8bc5af083aae321d57975c7/logo/orb.png?raw=true)

Orbmacs is an emacs configuration featuring Zettelkasten, completion features, avy, email, music, and some useful key chords & "sane defaults".

Packages are managed using [leaf](https://github.com/conao3/leaf.el).

Emacs is a never-ending journey and as such, my setup is always evolving. I would encourage forking this repository if you are interested in using it as a base for your own customizations, but ideally you should build your own configuration from scratch and simply pick out any parts you like.

If you have any questions or suggestions please feel free to contact me, and I will do my best to respond promptly.

# Installation
Run in a terminal
`git clone https://github.com/kvvba/orbmacs.git ~/.emacs.d`

Either restart emacs at this point or simply use `M-x load-file`, and point it to `~/.emacs.d/init.el`

# Notes

I'm using the GTD method of organisation. If you do not like GTD, remove the parts relating to it in: agenda files, refile targets, and capture templates.

I am using the "Hack" font available from NerdFonts. If you do not have this font or do not wish to use it, please change it to something else.

I am using leaf to manage my packages. If leaf is not to your taste it can easily be converted back to use-package.

If you wish to use mu4e for email, you will need to do some additional setup. There are many [excellent guides online](https://miikanissi.com/blog/email-setup-with-mbsync-mu4e) to help you. I also provide a template for my email settings with my personal details removed.

Default folders (you need to create these yourself if you do not have them)
- Org agenda: `~/org/`
- Org journal: `~/org/journal`
- Org roam: `~/RoamNotes/`
- Bongo: `~/Music/`

# TODO
- Clean up init.el
- Documentation
- Pomodoro timer
