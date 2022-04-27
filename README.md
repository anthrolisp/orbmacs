# Orbmacs

![O R B](https://github.com/kvvba/orbmacs/blob/ed9e8d190ea64375a8bc5af083aae321d57975c7/logo/orb.png?raw=true)

Orbmacs is an Emacs configuration featuring:
- Declarative package management (leaf.el)
- Zettelkasten (org roam)
- Journalling (org roam)
- Completion features (vertico, consult, company-mode)
- IDE (lsp-mode)
- Advanced movement (avy)
- Email (mu4e)
  - Emails alerts in modeline and as notification (mu4e-alert)
- Music with dired integration (bongo)
- Reference management (citar)
- Better window switching (ace-window)
- Better in-document search (swiper)
- Customized splash screen (dashboard)
- Git porcelain (magit)
- Dired commands to zip and unzip files
- Org mode improvements for writing
- Other useful bits and pieces

It is a relatively simple configuration I consider to be "comfy", and which allows me to accomplish most of my computing tasks without leaving Emacs.

Unfortunately there are presently no orbs in orbmacs aside from the dashboard image, but I am looking into ways to resolve this issue.

Packages are managed using [leaf](https://github.com/conao3/leaf.el).

I would encourage forking this repository if you are interested in using it as a base for your own customizations, but ideally you should build your own configuration from scratch and simply pick out any parts you like.

In the future I intend to add more features for python programming, specifically with numpy, and tweaks to org/org-roam and dired.

If you have any questions or suggestions please feel free to contact me, and I will do my best to respond promptly. A fair warning that I am a mechanical engineer and not a programmer by trade.

# Installation
Run in a terminal
`git clone https://github.com/kvvba/orbmacs.git ~/.emacs.d`

Either restart Emacs at this point or simply use `M-x load-file`, and point it to `~/.emacs.d/init.el`

# Hints

To view a range of help options, key in `C-h ?` or simply `M-x help`. Alternatively you can key `C-h` and wait a moment, and the package which-key will display a list of valid completions for different help options. `C-h f` (describe-function) and `C-h v` (describe-variable) are particularly helpful.

# Notes

I'm currently using the "getting things done" (GTD) method of organisation. If you do not like GTD, remove the parts relating to it in: agenda files, refile targets, and capture templates.

I'm using the "Hack" font available from NerdFonts. If you do not have this font or do not wish to use it, please change it to something else or comment out the setting to use the default.

I'm using leaf to manage my Emacs packages. If you prefer use-package, it's very quick and easy to change it over.

If you wish to use mu4e for email, you will need to do some additional setup. There are many [excellent guides online](https://miikanissi.com/blog/email-setup-with-mbsync-mu4e) to help you. I also provide a template for my email settings (for multiple accounts using mu4e contexts) with my personal details removed.

Default folders (you need to create these yourself if you do not have them)
- Org agenda: `~/org/`
- GTD: `~/org/gtd/`
- Org roam: `~/RoamNotes/`
- Bongo: `~/Music/`
- mu4e: `~/.mail/`
- Citar: `~/org/papers/bibliography.bib`

# Wish list
- Documentation
- Pomodoro timer
- Orb mode (???)
