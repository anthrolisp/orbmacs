# Orbmacs

![O R B](https://github.com/kvvba/orbmacs/blob/ed9e8d190ea64375a8bc5af083aae321d57975c7/logo/orb.png?raw=true)

Orbmacs is an emacs configuration featuring Zettelkasten (org roam), completion features (ivy), avy, email, music, and some useful key chords & "sane defaults". It is a relatively simple configuration I consider to be "comfy", and which allows me to accomplish most of my computing tasks. Unfortunately there are presently no orbs in orbmacs aside from the dashboard image, but I am looking into ways to resolve this issue.

Packages are managed using [leaf](https://github.com/conao3/leaf.el).

Emacs is a never-ending journey and as such, my setup is always evolving. I would encourage forking this repository if you are interested in using it as a base for your own customizations, but ideally you should build your own configuration from scratch and simply pick out any parts you like.

If you have any questions or suggestions please feel free to contact me, and I will do my best to respond promptly. A disclaimer that I am a mechanical engineer and not a programmer by trade.

# Installation
Run in a terminal
`git clone https://github.com/kvvba/orbmacs.git ~/.emacs.d`

Either restart emacs at this point or simply use `M-x load-file`, and point it to `~/.emacs.d/init.el`

# Hints

Use `C-h b` to view the help on emacs bindings, including ones created by packages and by me.

# Notes

I'm currently using the "getting things done" (GTD) method of organisation. If you do not like GTD, remove the parts relating to it in: agenda files, refile targets, and capture templates.

I'm using the "Hack" font available from NerdFonts. If you do not have this font or do not wish to use it, please change it to something else or comment out the setting to use the default.

I'm using leaf to manage my emacs packages. If you prefer use-package, it's very quick and easy to change it over.

If you wish to use mu4e for email, you will need to do some additional setup. There are many [excellent guides online](https://miikanissi.com/blog/email-setup-with-mbsync-mu4e) to help you. I also provide a template for my email settings (for multiple accounts using mu4e contexts) with my personal details removed.

Default folders (you need to create these yourself if you do not have them)
- Org agenda: `~/org/`
- Org journal: `~/org/journal/`
- GTD: `~/org/gtd/`
- Org roam: `~/RoamNotes/`
- Bongo: `~/Music/`
- mu4e: `~/.mail/`

# TODO
- Clean up init.el
- Documentation
- Pomodoro timer
- Orb mode (???)
