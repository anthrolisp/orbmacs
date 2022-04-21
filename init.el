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

;; Make emacs startup faster
(setq gc-cons-threshold 402653184
			gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
				gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

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
(global-set-key (kbd "C-c e") 'eshell)

(require 'cl-lib)

(setq inferior-lisp-program "sbcl")

;; Octave
(setq octave-comment-char ?%)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(leaf svg-tag-mode
	:ensure t)

;; Org mode

(require 'org)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
					'(lambda ()
						 (visual-line-mode 1)))
(add-hook 'org-mode-hook 'turn-on-flyspell)
(setq org-pretty-entities t)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(global-set-key (kbd "C-c c") 'org-capture)

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
(global-set-key (kbd "C-c a") 'org-agenda)
(setq recentf-exclude '("\\.org\\"))

(setq org-todo-keywords
			'((sequence "TODO" "PROG" "|" "DONE" "CANC")))

(setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")
(global-set-key (kbd "C-c t s") 'org-timer-set-timer)
(global-set-key (kbd "C-c t k") 'org-timer-stop)
(global-set-key (kbd "C-c t p") 'org-timer-pause-or-continue)

(add-to-list 'org-entities-user
						 '("oint","\\oint{}" t "&#8750" "..." "..." "∮"))

(setq org-refile-targets '(("~/org/gtd/reminders.org" :maxlevel . 2)
													 ("~/org/gtd/someday.org" :level . 1)
													 ("~/org/gtd/corkboard.org" :maxlevel . 3)))

;; Change parent task to done when all subtasks are marked as done
(defun org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(load "~/.emacs.d/mail.el")

(leaf mu4e-alert
	:ensure t
	:config
	(mu4e-alert-set-default-style 'libnotify)
	:init
	(mu4e-alert-enable-mode-line-display)
	(mu4e-alert-enable-notifications))

(load "~/.emacs.d/music.el")

(defun kill-dired-buffers ()
	(interactive)
	(mapc (lambda (buffer) 
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
            (kill-buffer buffer))) 
        (buffer-list)))

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

(leaf lsp-ivy
	:ensure t)

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
	:init
	(defhydra hydra-dailies (:hint nil :exit t)
		"
org roam dailies

[_j_]: capture today        [_J_]: goto today
[_y_]: capture yesterday    [_Y_]: goto yesterday
[_d_]: capture date         [_D_]: goto date

"
		("j" org-roam-dailies-capture-today)
		("J" org-roam-dailies-goto-today)
		("y" org-roam-dailies-capture-yesterday)
		("Y" org-roam-dailies-goto-yesterday)
		("d" org-roam-dailies-capture-date)
		("D" org-roam-dailies-goto-date)
		("q" nil "abort"))
	(global-set-key (kbd "C-c n d") 'hydra-dailies/body)
	:config
	(setq org-roam-completion-everywhere t)
	(setq org-roam-dailies-directory "~/RoamNotes/daily/")
	(setq org-roam-node-display-template
				(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)

	(defun org-roam-node-insert-immediate (arg &rest args)
		(interactive "P")
		(let ((args (cons arg args))
					(org-roam-capture-templates (list (append (car org-roam-capture-templates)
																										'(:immediate-finish t)))))
			(apply #'org-roam-node-insert args)))

	(setq org-roam-dailies-capture-templates
				'(("d" "default" entry "* %<%R>\n%?"
					 :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
	
	(setq org-roam-capture-templates
				'(("m" "main" plain
					 "%?"
					 :if-new (file+head "main/${slug}.org"
															"#+title: ${title}\n")
					 :immediate-finish t
					 :unnarrowed t)
					("r" "reference" plain "%?"
					 :if-new
					 (file+head "reference/${title}.org" "#+title: ${title}\n")
					 :immediate-finish t
					 :unnarrowed t)
					("u" "university" plain
					 "%?"
					 :if-new (file+head "university/${slug}.org" "#+title: ${title}\n")
					 :immediate-finish t
					 :unarrowed t)))
	
	(cl-defmethod org-roam-node-type ((node org-roam-node))
		"Return the TYPE of NODE."
		(condition-case nil
				(file-name-nondirectory
				 (directory-file-name
					(file-name-directory
					 (file-relative-name (org-roam-node-file node) org-roam-directory))))
			(error ""))))

(leaf ivy
	:ensure t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
	(setq ivy-re-builders-alist
				'((t . ivy--regex-ignore-order))))

(leaf counsel
	:ensure t
  :init
  (counsel-mode 1))

(leaf htmlize
	:ensure t)

(leaf diminish
	:ensure t)

(leaf which-key
	:ensure t
  :init
  (which-key-mode))

(leaf swiper
	:ensure t
  :bind ("C-s" . 'swiper))

(leaf beacon
	:ensure t
  :diminish beacon-mode
  :init
  (beacon-mode 1))

(leaf avy
	:ensure t
  :bind
  ("M-s" . avy-goto-char)
	("C-c l" . swiper-avy))

(leaf switch-window
	:ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
				'("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

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
	(load-theme 'doom-homage-white))

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




















