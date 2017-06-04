;; elpa packages
(require 'package)
(dolist (source '(("melpa-stable" . "https://stable.melpa.org/packages/")
				  ("melpa" . "https://melpa.org/packages/")
				  ("marmalade" . "https://marmalade-repo.org/packages/")
				  ("gnu" . "https://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives source t))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
		("melpa" . 15)
        ("marmalade" . 10)
        ("gnu" . 5)))
(package-initialize)
(setq package-enable-at-startup nil)

;; define load paths
(setq default-directory (substitute-in-file-name "$HOME/"))
(defconst elpa-dir (substitute-in-file-name "$HOME/.emacs.d/elpa"))
(defconst nonelpa-dir (substitute-in-file-name "$HOME/.emacs.d/nonelpa"))
(defconst local-dir (substitute-in-file-name "$HOME/.emacs.d/local"))
(add-to-list 'load-path elpa-dir)
(add-to-list 'load-path nonelpa-dir)
(add-to-list 'load-path local-dir)

;; key-bindings independent of packages
(load "kbd")

;; utils
(load "utils")

;; alias UTF-8 as utf-8
(define-coding-system-alias 'UTF-8 'utf-8)

;; make scoll smooth
(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 10000)

;; making buffers with the same name distinguishable
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; disable start-up message
(setq inhibit-startup-message t)

;; disable tool-bar
(tool-bar-mode -1)

;; disable menu-bar
;; (menu-bar-mode -1)

;; disable side-bar
(scroll-bar-mode -1)

;; do not show mode line
(setq-default mode-line-format nil)

;; display line numbers
;; (require 'linum)
;; (global-linum-mode t) ;; always show line numbers
;; (setq linum-format "%4d| ") ;; linum format

;; tab indent
(setq-default c-basic-offset 4
              tab-width 4)
(defun my-indent-setup()
  (c-set-offset 'arglist-intro '+))
(add-hook 'c++-mode-hook 'my-indent-setup)
(add-hook 'c-mode-hook 'my-indent-setup)
(setq python-indent-offset 4)

;; use y/n to replace yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; no back-up or auto-save files
(setq make-backup-files nil) ;; no backup files
(setq auto-save-mode nil) ;; no auto save
(setq-default make-backup-files nil) ;; no temp file
(setq backup-inhibited t) ;; no backup
(setq auto-save-default nil) ;; no #file#

;; show paren match
(show-paren-mode 1)

;; split windows vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; load customize variables
(setq custom-file (concat local-dir "/custom.el"))
(add-hook 'emacs-startup-hook
		  (lambda ()
			(load custom-file)
			(hs-sort-package-list)))

;; sort package-selected-list
(hs-sort-package-list)
(add-hook 'kill-emacs-hook 'hs-sort-package-list)

;; extra config
(setq extra-config-file (concat local-dir "/extra-config.el"))
(load extra-config-file)
