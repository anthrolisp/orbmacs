;;;; orbmacs --- General purpose Emacs configuration for engineers

;; Copyright (c) 2022 jakub@posteo.net

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
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
		;; (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

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

	(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

	;; Show line numbers
	(add-hook 'prog-mode-hook 'display-line-numbers-mode)
	(add-hook 'text-mode-hook 'display-line-numbers-mode)

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
	(defalias 'yes-or-no-p 'y-or-n-p)

	;; Rebind keys for resizing
	(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
	(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
	(global-set-key (kbd "s-C-<down>") 'shrink-window)
	(global-set-key (kbd "s-C-<up>") 'enlarge-window)

	;; Highlight current line
	(global-hl-line-mode t)

	(set-face-attribute 'default nil
											:family "Hack Nerd Regular"
											:height 100
											:weight 'normal
											:width 'normal)

	;; Aliases
	(defalias 'open 'find-file-other-window)
	(defalias 'clean 'eshell/clear-scrollback)

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

	(defun concat-string-list (list)
		"Return a string which is a concatenation of all elements of the list separated by spaces"
    (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

	(require 'cl-lib)

	(setq inferior-lisp-program "sbcl")

	(setq enable-recursive-minibuffers t))

(leaf octave
	:config
	(setq octave-comment-char ?%)
	(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(leaf eshell
	:defer-config
	;; Eshell
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

	(global-set-key (kbd "<s-C-return>") 'eshell-other-window)
	(global-set-key (kbd "C-c e") 'eshell))

(leaf org
	:ensure t
	:config
	(setq org-pretty-entities t)
	(setq org-capture-templates '(("t" "Todo [inbox]" entry
																 (file+headline "~/org/gtd/inbox.org" "Tasks")
																 "* TODO %i%?")
																("n" "Note" entry
																 (file+headline "~/org/gtd/inbox.org" "Notes")
																 "* %(read-string\"Title: \") %T\n%i%?")
																("r" "Reminder" entry
																 (file+headline "~/org/gtd/reminders.org" "Reminders")
																 "* %i%?\n%U")
																("m" "Meeting minutes" entry
																 (file+headline "~/org/meetings.org" "Meeting notes")
																 "* Meeting title: %(read-string \"Meeting title: \")\nAttending: Jakub Cranmer, %(read-string \"Attendees: \")\nTime: %U\n\n%i%?")))
	(setq org-agenda-files '("~/org/gtd/inbox.org"
													 "~/org/gtd/corkboard.org"
													 "~/org/gtd/reminders.org"
													 "~/org/timetable.org"))
	(setq recentf-exclude '("\\.org\\"))
	(setq org-todo-keywords
				'((sequence "TODO" "PROG" "|" "DONE" "CANC")))
	(setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")
	(defun org-summary-todo (n-done n-not-done)
		(let (org-log-done org-log-states)   ; turn off logging
			(org-todo (if (= n-not-done 0) "DONE" "TODO"))))
	(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
	(setq org-refile-targets '(("~/org/gtd/reminders.org" :maxlevel . 2)
														 ("~/org/gtd/someday.org" :level . 1)
														 ("~/org/gtd/corkboard.org" :maxlevel . 3)))
	(global-set-key (kbd "C-c c") 'org-capture)
	(global-set-key (kbd "C-c a") 'org-agenda)
	(global-set-key (kbd "C-c t s") 'org-timer-set-timer)
	(global-set-key (kbd "C-c t k") 'org-timer-stop)
	(global-set-key (kbd "C-c t p") 'org-timer-pause-or-continue)
	(add-to-list 'org-entities-user
							 '("oint","\\oint{}" t "&#8750" "..." "..." "∮"))
	:hook
	(org-mode . org-indent-mode)
	(org-mode . (lambda ()
								(visual-line-mode 1)))
	(org-mode . turn-on-flyspell)
	(org-after-todo-statistics . org-summary-todo))

(leaf citar
	:ensure t
  :bind (("C-c b" . citar-insert-citation)
         (minibuffer-local-map
					("M-b" . citar-insert-preset)))
  :custom
  (citar-bibliography . '("~/org/papers/references.bib")))

(load "~/.emacs.d/mail.el")

(leaf mu4e-alert
	:ensure t
	:after mu4e
	:config
	(mu4e-alert-set-default-style 'libnotify)
	(mu4e-alert-enable-mode-line-display)
	(mu4e-alert-enable-notifications))

(leaf bongo
	:ensure t
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
  :config
  (setq vterm-timer-delay 0.01)
	(global-set-key (kbd "C-c v") 'vterm))

(leaf slime
	:ensure t)

(leaf slime-company
	:ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(leaf all-the-icons
	:ensure t
  :if (display-graphic-p))

(leaf lsp-mode
	:ensure t
  :commands lsp
  :custom
  (lsp-eldoc-render-all . t)
  (lsp-idle-delay . 0.6)
  ;; enable / disable the hints as you prefer:
  :hook
  (lsp-mode-hook . lsp-ui-mode))

(leaf lsp-ui
	:ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-doc-enable . nil))

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

(leaf org-roam
  :ensure t
  :custom
  (org-roam-directory . "~/RoamNotes/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
				 ("C-c n f" . org-roam-node-find)
				 ("C-c n g" . org-roam-graph)
				 ("C-c n i" . org-roam-node-insert)
				 ("C-c n I" . org-roam-node-insert-immediate)
				 ("C-c n c" . org-roam-capture))
	:config
	(load-file "~/.emacs.d/roam.el"))

(leaf swiper
	:ensure t
  :bind (("C-s" . swiper)
				 ("C-c l" . swiper-avy)))

(leaf vertico
	:ensure t
	:init
	(vertico-mode))

(leaf consult
	:ensure t)

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

(leaf avy
	:ensure t
  :bind
  ("M-s" . avy-goto-char))

(leaf ace-window
	:ensure t
	:config
	(global-set-key (kbd "C-x o") 'ace-window)
	(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(leaf async
	:ensure t
  :init
  (dired-async-mode 1))

(leaf page-break-lines
	:ensure t
  :diminish (page-break-lines-mode visual-line-mode))

(leaf undo-tree
	:ensure t
  :diminish undo-tree-mode)

(leaf magit
	:ensure t)

(leaf eldoc
	:ensure t
  :diminish eldoc-mode)

(leaf doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
	:init
	(setq custom-safe-themes t)
	(load-theme 'doom-acario-light))

(leaf moody
	:ensure t
  :init
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; (leaf minions
;; 	:ensure t
;; 	:init (minions-mode 1))

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
  (setq dashboard-items '((recents . 5)
													(bookmarks . 9)))
  (add-to-list 'dashboard-items '(agenda) t)
  (setq deashboard-week-agenda t)
  (setq dashboard-banner-logo-title "O R B M A C S")
  (setq dashboard-startup-banner "~/.emacs.d/media/orb.png")
  (setq dashboard-center-content t)
  ;; (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time))))

(provide 'init)
;;; init.el ends here
