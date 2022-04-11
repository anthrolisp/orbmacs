;; Initialize melpa repo
(require 'package)
(setq package-archives nil)
(add-to-list 'package-archives
        '("melpa" . "https://melpa.org/packages/")
		'("gnu" . "https://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(setq x-select-enable-clipboard t)

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

;; Change parent task to done when all subtasks are marked as done
(defun org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; Make [/] and [%] include *all* subtasks recusively
;; (setq org-hierarchical-todo-statistics nil)

;; Defer loading most packages for quicker startup
(setq use-package-always-defer t)

;; Diable default startup screen
(setq inhibit-startup-message t)

(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq recentf-excluse '("~/org"))
(setq org-todo-keywords
	  '((sequence "TODO" "PROG" "|" "DONE")))

;; Set default alarm sound for end of timer
(setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")
(global-set-key (kbd "C-c t s") 'org-timer-set-timer)
(global-set-key (kbd "C-c t k") 'org-timer-stop)
(global-set-key (kbd "C-c t p") 'org-timer-pause-or-continue)

;; Set default font to hack
(setq default-frame-alist '((font . "Hack Nerd Font Mono-10")))

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
  "Open a file as root in Eshell."
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

(global-set-key (kbd "C-c v") 'vterm)

(require 'cl-lib)

(setq inferior-lisp-program "sbcl")

;; Octave
(setq octave-comment-char ?%)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Packages

(require 'org)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
	'(lambda ()
	   (visual-line-mode 1)))
(setq org-pretty-entities t)

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(load "~/.emacs.d/mail.el")
(global-set-key (kbd "C-c m" 'mu4e))

(use-package svg-tag-mode
  :ensure t)

(use-package vterm
  :ensure t
  :config
  (setq vterm-timer-delay 0.01))

(use-package slime
  :ensure t)

(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(setq lsp-rust-server 'rust-analyzer)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-ivy
  :ensure t)

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  (global-company-mode t)
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package org-journal
  :ensure t
  :defer t
  :bind (("C-c C-j" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n i" . org-roam-node-insert)
		 :map org-mode-map
		 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :init
  (counsel-mode 1))

(use-package htmlize
  :ensure t)

(use-package diminish
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (beacon-mode 1))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(use-package switch-window
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

(use-package async
  :ensure t
  :init
  (dired-async-mode 1))

(use-package page-break-lines
  :ensure t
  :diminish (page-break-lines-mode visual-line-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package magit
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode)

(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (load-theme 'doom-solarized-light)

;; (use-package nix-mode
;;   :mode "\\.nix\\'")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dashboard
  :ensure t
  :defer nil
  :preface
  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 9)
						  (bookmarks . 5)))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-oceanic-next))
 '(package-selected-packages
	 '(mu4e yasnippet which-key vterm use-package undo-tree switch-window slime rustic powerline page-break-lines org-roam org-journal neotree magit lsp-ui lsp-ivy htmlize flycheck doom-themes diminish dashboard counsel company beacon avy async all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
