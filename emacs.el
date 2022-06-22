;;;; orbmacs --- Emacs configuration for org mode

;; Copyright (c) 2022 jakub@posteo.net

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "software"), to deal
;; in the software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the software, and to permit persons to whom the software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Orbmacs is an Emacs configuration with a focus on org mode and "living in Emacs"

;;; Code:

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
											 ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(leaf emacs
	:init
	;; Make emacs start faster
	(setq startup/gc-cons-threshold gc-cons-threshold)
	(setq gc-cons-threshold most-positive-fixnum)
	(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))

	(defvar startup/file-name-handler-alist file-name-handler-alist)
	(setq file-name-handler-alist nil)

	(defun startup/revert-file-name-handler-alist ()
		(setq file-name-handler-alist startup/file-name-handler-alist))

	;; Show parent brackets
	(show-paren-mode 1)

	;; Diable GUI elements
	(tool-bar-mode -1)
	(menu-bar-mode -1)
	(scroll-bar-mode -1)

	;; Copy paste outside of emacs
	(setq select-enable-clipboard t)

	;; Disable automatic backup files
	(setq make-backup-files nil)
	(setq auto-save-default nil)

	;; Enable conservative scrolling
	(setq scroll-conservatively 100)

	;; Diable ring-bell
	;; (setq ring-bell-function 'ignore)

	;; Indentation
	(setq-default tab-width 2)
	(setq-default standard-indent 2)
	(setq c-basic-offset tab-width)
	(setq-default electric-indent-inhibit t)
	(setq-default indent-tabs-mode t)
	(setq backward-delete-char-untabify-method 'nil)

	;; Enable prettify symbols
	;; (global-prettify-symbols-mode t)

	;; Enable bracket pair-matching
	(setq electric-pair-pairs '(
															(?\{ . ?\})
															(?\( . ?\))
															(?\[ . ?\])
															(?\" . ?\")
															))
	(electric-pair-mode t)

	;; Cursor follows new window
	(defun split-and-follow-horizontally ()
		(interactive)
		(split-window-below)
		(balance-windows)
		(other-window 1))
	(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

	(defun split-and-follow-vertically ()
		(interactive)
		(split-window-right)
		(balance-windows)
		(other-window 1))
	(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

	;; Turn yes-or-no to y-or-n
	(setq use-short-answers t)

	;; Rebind keys for resizing
	(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
	(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
	(global-set-key (kbd "s-C-<down>") 'shrink-window)
	(global-set-key (kbd "s-C-<up>") 'enlarge-window)

	;; Highlight current line
	(global-hl-line-mode t)

	(set-face-attribute 'default nil
											:family "FiraMono Nerd Font"
											;; :height 100
											;; :weight 'normal
											;; :width 'normal
											)

	(defun concat-string-list (list)
		"Return a string which is a concatenation of all elements of the list separated by spaces"
    (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

	(require 'cl-lib)

	(setq inferior-lisp-program "sbcl")

	(setq enable-recursive-minibuffers t)
	:bind
	;; Hitting suspend frame by accident is annoying me
	("C-z" . nil)
	("C-x C-z" . nil)
	("H-b" . consult-buffer)
	("H-B" . consult-buffer-other-window)
	("H-k" . kill-buffer)
	:hook
	(emacs-startup-hook . startup/revert-file-name-handler-alist)
	(prog-mode-hook . display-line-numbers-mode)
	(text-mode-hook . display-line-numbers-mode))

(leaf octave
	:leaf-defer t
	:defer-config
	(setq octave-comment-char ?%)
	(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(leaf eshell
	:leaf-defer t
	:defer-config
	(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
	(setq eshell-prompt-function
				(lambda nil
					(concat
					 (if (string= (eshell/pwd) (getenv "HOME"))
							 (propertize "~" 'face `(:foreground "#268bd2"))
						 (replace-regexp-in-string
							(getenv "HOME")
							(propertize "~" 'face `(:foreground "#268bd2"))
							(propertize (eshell/pwd) 'face `(:foreground "#268bd2"))))
					 (if (= (user-uid) 0)
							 (propertize " α " 'face `(:foreground "#d33682"))
						 (propertize " λ " 'face `(:foreground "#d33682"))))))

	(setq eshell-highlight-prompt nil)

	;; Aliases
	(defalias 'open 'find-file-other-window)
	(defalias 'clean 'eshell/clear-scrollback)

	(defun eshell/sudo-open (filename)
		"Open a file (FILENAME) as root in Eshell."
		(let ((qual-filename (if (string-match "^/" filename)
														 filename
													 (concat (expand-file-name (eshell/pwd)) "/" filename))))
			(switch-to-buffer
			 (find-file-noselect
				(concat "/sudo::" qual-filename)))))

	(defun eshell-other-window ()
		"Create or visit an eshell buffer."
		(interactive)
		(if (not (get-buffer "*eshell*"))
				(progn
					(split-window-sensibly (selected-window))
					(other-window 1)
					(eshell))
			(switch-to-buffer-other-window "*eshell*")))
	:bind
	("<s-C-return>" . eshell-other-window)
	("C-c e" . eshell))

(leaf dired
	:defer-config
		(defun kill-dired-buffers ()
		(interactive)
		(mapc (lambda (buffer)
						(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
							(kill-buffer buffer)))
					(buffer-list)))

	(eval-after-load "dired-aux"
		'(add-to-list 'dired-compress-file-suffixes
									'("\\.zip\\'" ".zip" "unzip")))
	(eval-after-load "dired"
		'(define-key dired-mode-map "z" 'dired-zip-files))

	(defun dired-zip-files (zip-file)
		"Create an archive containing the marked files."
		(interactive "sEnter name of zip file: ")
		;; create the zip file
		(let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
			(shell-command
			 (concat "zip "
							 zip-file
							 " "
							 (concat-string-list
								(mapcar
								 '(lambda (filename)
										(file-name-nondirectory filename))
								 (dired-get-marked-files))))))

		(revert-buffer)

		;; remove the mark on all the files  "*" to " "
		;; (dired-change-marks 42 ?\040)
		;; mark zip file
		;; (dired-mark-files-regexp (filename-to-regexp zip-file))
		)
	:bind
	("H-d" . dired)
	)

(leaf meow
	:ensure t
	:init
	(load-file "~/.emacs.d/meow.el")
	(meow-setup)
	(meow-global-mode t)
	(meow-define-keys
			'normal
		'("P" . consult-yank-pop)))

(leaf dired-narrow
	:after dired
	:ensure t
	:bind ((dired-mode-map
					:package dired
					("/" . dired-narrow))))

(leaf org
	:ensure t
	:config
	(setq sentence-end-double-space nil)
	(setq org-pretty-entities t)
	
	(defun org-capture-mail ()
		(interactive)
		(call-interactively 'org-store-link)
		(org-capture nil "@"))
	
	(setq org-capture-templates '(("i" "Inbox" entry
																 (file+headline "~/org/gtd/inbox.org" "Tasks")
																 "* TODO %i%?\nEntered on: %U")
																("@" "Inbox [mu4e]" entry
																 (file+headline "~/org/gtd/inbox.org" "Mail")
																 "* TODO Process \"%a\" %?\nEntered on: %U")
																("N" "Notebook" entry
																 (file "~/org/notes/notebook.org")
																 "* %(read-string\"Title: \")\nEntered on: %U\n%i%?")
																("r" "Reminder" entry
																 (file+headline "~/org/gtd/reminders.org" "Reminders")
																 "* TODO %i%?\nEntered on: %U")
																("m" "Meeting minutes" entry
																 (file+headline "~/org/meetings.org" "Meeting notes")
																 "* Meeting title: %(read-string \"Meeting title: \")\nAttending: %(read-string \"Attendees: \")\nTime: %U\n\n%i%?")))
	
	(setq org-agenda-files '("~/org/gtd/inbox.org"
													 "~/org/gtd/corkboard.org"
													 "~/org/gtd/reminders.org"
													 "~/org/timetable.org"))
	
	(setq recentf-exclude '("\\.org\\"))
	(setq org-todo-keywords
				'((sequence "TODO" "|" "DONE" "CANC")))
	(setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")

	(defun org-summary-todo (n-done n-not-done)
		(let (org-log-done org-log-states)   ; turn off logging
			(org-todo (if (= n-not-done 0) "DONE" "TODO"))))

	(setq org-refile-targets '(("~/org/gtd/reminders.org" :maxlevel . 2)
														 ("~/org/gtd/someday.org" :level . 1)
														 ("~/org/gtd/corkboard.org" :maxlevel . 3)))
	(add-to-list 'org-entities-user
							 '("oint","\\oint{}" t "&#8750" "..." "..." "∮"))
	(with-eval-after-load 'ox-latex
		(add-to-list 'org-latex-classes
							 '("elsarticle"
								 "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
								 ("\\section{%s}" . "\\section*{%s}")
								 ("\\subsection{%s}" . "\\subsection*{%s}")
								 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
								 ("\\paragraph{%s}" . "\\paragraph*{%s}")
								 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
	:bind
	("C-c c" . org-capture)
	("C-c a" . org-agenda)
	("C-c t s" . org-timer-set-timer)
	("C-c t k" . org-timer-stop)
	("C-c t p" . org-timer-pause-or-continue)
	("C-c b" . org-cite-insert)
	:hook
	(org-mode-hook . flyspell-mode)
	(org-mode-hook . org-indent-mode)
	(org-mode-hook . visual-line-mode)
	(org-after-todo-statistics-hook . org-summary-todo))

(leaf auctex
	:ensure t)

(leaf leaf
  :straight (org-pandoc-import
						 :type git
						 :host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))

(leaf olivetti
	:ensure t
	:config
	(setq olivetti-style 'fancy)
	:bind
	("C-c O" . olivetti-mode)
	:hook
	(olivetti-mode-hook . (lambda ()(setq olivetti-body-width 0.5))))

(leaf page-break-lines
	:ensure t
	:init
	(global-page-break-lines-mode))

(leaf citar
	:ensure t
	:after org
  :custom
  (org-cite-global-bibliography . '("~/org/papers/references.bib"))
	(org-cite-insert-processor . 'citar)
  (org-cite-follow-processor . 'citar)
  (org-cite-activate-processor . 'citar)
  (citar-bibliography . org-cite-global-bibliography)
	:config
	(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
	(setq citar-at-point-function 'embark-act))

(leaf mu4e
	:leaf-defer t
	:bind
	("C-c z" . mu4e)
	("C-c Z" . mu4e-other-window)
	(mu4e-main-mode-map
	 ("e" . kill-current-buffer))
	(mu4e-headers-mode-map
	 ("C-c c" . org-capture-mail))
	(mu4e-view-mode-map
	 ("C-c c" . org-capture-mail))
	:defer-config
	(load "~/.emacs.d/mail.el"))

(leaf mu4e-alert
	:ensure t
	:after mu4e
	:config
	(mu4e-alert-set-default-style 'libnotify)
	(mu4e-alert-enable-mode-line-display)
	(mu4e-alert-enable-notifications))

(leaf elfeed
	:ensure t
	:leaf-defer t
	:defer-config
	(setq elfeed-feeds
				'(
					;; emacs
					("https://masteringemacs.org/feed" emacs)
					("https://rss.sciencedirect.com/publication/science/03019322" fluids multiphase)
					("https://rss.sciencedirect.com/publication/science/13594311" fluids thermal)
					("https://rss.sciencedirect.com/publication/science/0142727X" fluids thermal)
					("https://rss.sciencedirect.com/publication/science/00457930" fluids computation)
					("https://www.mdpi.com/rss/journal/fluids" fluids)
					))
	:bind
	("C-c w" . elfeed)
	(elfeed-search-mode-map
	 ("U" . elfeed-update)))

(leaf bongo
	:ensure t
	:leaf-defer t
	:defer-config (load-file "~/.emacs.d/music.el")
	:bind
	(("<C-XF86AudioPlay>" . bongo-pause/resume)
   ("<C-XF86AudioNext>" . bongo-next)
   ("<C-XF86AudioPrev>" . bongo-previous)
   ("<M-XF86AudioPlay>" . bongo-show)
   ("<S-XF86AudioNext>" . bongo-seek-forward-10)
   ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
	 ("C-c p"             . bongo)
	 (bongo-playlist-mode-map
		("n" . bongo-next-object)
		("p" . bongo-previous-object)
		("M-n" . prot/bongo-paylist-section-next)
		("M-p" . prot/bongo-paylist-section-previous)
		("M-h" . prot/bongo-playlist-mark-section)
		("M-d" . prot/bongo-playlist-kill-section)
		("g" . prot/bongo-playlist-reset)
		("D" . prot/bongo-playlist-terminate)
		("r" . prot/bongo-playlist-random-toggle)
		("R" . bongo-rename-line)
		("j" . bongo-dired-line)       ; Jump to dir of file at point
		("J" . dired-jump)             ; Jump to library buffer
		("i" . prot/bongo-playlist-insert-playlist-file)
		("I" . bongo-insert-special))
	 (bongo-dired-library-mode-map
		("<C-return>" . prot/bongo-dired-insert)
		("C-c SPC" . prot/bongo-dired-insert)
		("C-c +" . prot/bongo-dired-make-playlist-file))))

(leaf vterm
	:ensure t
	:leaf-defer t
  :config
  (setq vterm-timer-delay 0.01)
	(global-set-key (kbd "C-c v") 'vterm))

(leaf slime
	:ensure t
	:leaf-defer t)

(leaf slime-company
	:ensure t
	:leaf-defer t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(leaf all-the-icons
	:ensure t
  :if (display-graphic-p))

(leaf all-the-icons-dired
	:ensure t
	:if (display-graphic-p)
	:leaf-defer t
	:commands all-the-icons-dired-mode
	:hook (dired-mode-hook . all-the-icons-dired-mode))

;; (leaf lsp-mode
;; 	:ensure t
;;   :commands lsp
;;   :custom
;;   (lsp-eldoc-render-all . t)
;;   (lsp-idle-delay . 0.6)
;;   ;; enable / disable the hints as you prefer:
;;   :hook
;;   (lsp-mode-hook . lsp-ui-mode))

;; (leaf lsp-ui
;; 	:ensure t
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show . t)
;;   (lsp-ui-sideline-show-hover . t)
;;   (lsp-ui-doc-enable . nil))

(leaf eglot
	:ensure t
	:leaf-defer
	:hook ((c-mode-hook c++-mode-hook python-mode-hook) . eglot-ensure)
	:config
	(add-to-list 'exec-path (expand-file-name "~/.local/bin/"))
	(add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("ccls")))
	(add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))


(leaf company
	:ensure t
  :custom
  (company-idle-delay . 0.25) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  (global-company-mode . t)
	:config
	(add-to-list 'company-backends 'company-capf)
  :bind (company-active-map
	       ("C-n" . company-select-next)
	       ("C-p" . company-select-previous)
	       ("M-<" . company-select-first)
	       ("M->" . company-select-last)))

(leaf yasnippet
	:ensure t
	:hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode))

(leaf leaf
	:straight (denote
						 :type git
						 :host github
						 :repo "protesilaos/denote")
	:config
	(with-eval-after-load 'org-capture
  (require 'denote-org-capture)
  (add-to-list 'org-capture-templates
               '("n" "New note" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t))))

(leaf leaf
	:straight (consult-notes
						 :type git
						 :host github
						 :repo "mclear-tools/consult-notes")
	:config
	(setq consult-notes-data-dirs '(("notes" ?o "~/org/notes/")
																	("papers" ?p "~/org/papers/"))))

;; (leaf org-roam
;;   :ensure t
;;   :custom
;;   (org-roam-directory . "~/RoamNotes/")
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;; 				 ("C-c n f" . org-roam-node-find)
;; 				 ("C-c n g" . org-roam-graph)
;; 				 ("C-c n i" . org-roam-node-insert)
;; 				 ("C-c n I" . org-roam-node-insert-immediate)
;; 				 ("C-c n c" . org-roam-capture))
;; 	:config
;; 	(load-file "~/.emacs.d/roam.el"))

;; (leaf org-roam-ui
;; 	:ensure t
;; 	:after org-roam
;; 	:config
;; 	(setq org-roam-ui-sync-theme t
;; 				org-roam-ui-follow t
;; 				org-roam-ui-update-on-save t
;; 				org-roam-ui-open-on-start t)
;; 	:bind
;; 	("C-c n u" . org-roam-ui-mode))


(leaf vertico
	:ensure t
	:init
	(vertico-mode))

(leaf consult
	:ensure t
	:bind
	("H-s" . consult-line)
	("M-g g" . consult-goto-line)
	("C-x r b" . consult-bookmark))

;; (leaf consult-org-roam
;; 	:ensure t
;; 	;; :init
;; 	;; (consult-org-roam-mode 1)
;; 	;; :config
;; 	;; (consult-customize
;; 	;;  consult-org-roam-forward-links
;; 	;;  :preview-key (kbd "M-."))
;; 	:bind
;; 	("C-c n e" . consult-org-roam-file-find)
;; 	("C-c n b" . consult-org-roam-backlinks)
;; 	("C-c n r" . consult-org-roam-search)
;; 	;; :hook
;; 	;; (org-mode-hook . consult-org-roam-mode)
;; 	)


(leaf embark
	:ensure t
	:bind
	(("C-." . embark-act)         ;; pick some comfortable binding
	 ("C-;" . embark-dwim)        ;; good alternative: M-.
	 ("C-h b" . embark-bindings)) ;; alternative for `describe-bindings'
	:init
	;; Optionally replace the key help with a completing-read interface
	(setq prefix-help-command #'embark-prefix-help-command)
	:config
	;; Hide the mode line of the Embark live/completions buffers
	(add-to-list 'display-buffer-alist
							 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
								 nil
								 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
	:ensure t
	:after (embark consult)
	:leaf-defer nil
	:hook
	(embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf savehist
	:init
	(savehist-mode)
	:config
	(setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring)))

(leaf marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(leaf orderless
  :ensure t
	:after vertico
	:config
	(setq completion-styles '(orderless)))

(leaf htmlize
	:ensure t)

(leaf diminish
	:ensure t)

(leaf which-key
	:ensure t
  :init
  (which-key-mode))

(leaf beacon
	:ensure t
  :diminish beacon-mode
  :init
  (beacon-mode 1))

;; (leaf avy
;; 	:ensure t
;;   :bind
;;   ("M-s" . avy-goto-char))

(leaf ace-window
	:ensure t
	:config
	(global-set-key (kbd "H-o") 'ace-window)
	(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(leaf async
	:ensure t
  :init
  (dired-async-mode 1))

(leaf undo-tree
	:ensure t
  :diminish undo-tree-mode)

(leaf magit
	:ensure t
	:leaf-defer)

(leaf eldoc
	:ensure t
  :diminish eldoc-mode)

(leaf rainbow-delimiters
	:ensure t
	:hook
	(prog-mode-hook . rainbow-delimiters-mode))

;; (leaf leaf
;; 	:ensure t
;;   :straight (lambda-line :type git :host github :repo "lambda-emacs/lambda-line")
;;   :custom
;;   (lambda-line-position . 'bottom) ;; Set position of status-line
;;   (lambda-line-abbrev . t) ;; abbreviate major modes
;;   (lambda-line-hspace . "  ")  ;; add some cushion
;;   (lambda-line-prefix . t) ;; use a prefix symbol
;;   (lambda-line-prefix-padding . nil) ;; no extra space for prefix
;;   (lambda-line-status-invert . nil)  ;; no invert colors
;;   (lambda-line-gui-ro-symbol . " ⨂") ;; symbols
;;   (lambda-line-gui-mod-symbol . " ⬤")
;;   (lambda-line-gui-rw-symbol . " ◯")
;;   (lambda-line-space-top . +.1)  ;; padding on top and bottom of line
;;   (lambda-line-space-bottom . -.1)
;;   (lambda-line-symbol-position . 0.1) ;; adjust the vertical placement of symbol
;;   :config
;; 	(customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
;; 	(customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))
;;   ;; activate lambda-line
;;   (lambda-line-mode)
;;   ;; set divider line in footer
;;   (when (eq lambda-line-position 'top)
;;    (setq-default mode-line-format (list "%_"))
;;    (setq mode-line-format (list "%_"))))

(leaf leaf
  :straight (lambda-themes :type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments . t)
  (lambda-themes-set-italic-keywords . t)
  (lambda-themes-set-variable-pitch . t)
  :config
	(setq custom-safe-themes t)
  (load-theme 'lambda-light-faded))

;; (leaf nix-mode
;;   :mode "\\.nix\\'")

(leaf flycheck
	:ensure t
  :init (global-flycheck-mode))

(leaf dashboard
	:ensure t
  :leaf-defer nil
  :preface
  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
	(setq inhibit-startup-message t)
  (setq dashboard-items '((recents . 9)
													(bookmarks . 15)))
  ;; (add-to-list 'dashboard-items '(agenda) t)
  ;; (setq deashboard-week-agenda t)
  (setq dashboard-banner-logo-title "Welcome to Orbmacs.")
  ;; (setq dashboard-startup-banner "~/.emacs.d/media/orb.png")
  (setq dashboard-startup-banner "~/.emacs.d/media/sicp.png")
	;; (setq dash-board-startup-banner 'official)
  (setq dashboard-center-content t)
	;; (setq dashboard-set-heading-icons t)
	;; (setq dashboard-set-file-icons t)
  ;; (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time))))
(provide 'emacs)
;;; emacs.el ends here
