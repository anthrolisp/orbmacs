;; Initialize melpa repo
(require 'package)
(setq package-archives nil)
(add-to-list 'package-archives
        '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
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
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'nil)

;; Enable prettify symbols
(global-prettify-symbols-mode t)

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

(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq recentf-excluse '("~/org"))

;; Set default alarm sound for end of timer
(setq org-clock-sound "~/.emacs.d/media/digital_alarm.wav")

;; Set default font to hack
;; (set-frame-font "Hack Nerd Font Mono 10" nil t)

;; Aliases
(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

;; Eshell
(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
(setq eshell-prompt-function
	  (lambda nil
		(concat
		 (if (string= (eshell/pwd) (getenv "HOME"))
			 (propertize "~" 'face `(:foreground "#99CCFF"))
		   (replace-regexp-in-string
			(getenv "HOME")
			(propertize "~" 'face `(:foreground "#99CCFF"))
			(propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
		 (if (= (user-uid) 0)
			 (propertize " α " 'face `(:foreground "#FF6666"))
		   (propertize " λ " 'face `(:foreground "#A6E22E"))))))

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

(require 'cl-lib)

(setq inferior-lisp-program "sbcl")

;; Octave
(setq octave-comment-char ?%)

;; Packages

(require 'org)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
	'(lambda ()
	   (visual-line-mode 1)))
(setq org-pretty-entities t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(use-package slime
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
			(lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode))))

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

(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'arrow)

(use-package htmlize
  :ensure t)

(use-package diminish
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode)

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

(load-theme 'doom-solarized-dark)

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
 '(package-selected-packages
   '(rust-mode slime which-key use-package undo-tree switch-window powerline page-break-lines org-roam org-journal neotree magit htmlize flycheck doom-themes diminish dashboard counsel company beacon avy async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
