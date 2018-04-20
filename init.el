;; -*- mode: emacs-lisp -*-
;; This is the entry point of init code.
;; The code included here is about built-in config.
;; `utils` and `kbd` are both about built-in settings.
;; `extra-config` is about system-independent package-specific config.
;; `local-config-[OS]` is about system-specific config.

;; elpa packages
(require 'package)
(dolist (source '(("melpa" . "https://melpa.org/packages/")
				  ("melpa-stable" . "https://stable.melpa.org/packages/")
				  ("marmalade" . "https://marmalade-repo.org/packages/")
				  ("gnu" . "https://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives source t))
(setq package-archive-priorities
      '(("melpa" . 20)
		("melpa-stable" . 15)
        ("gnu" . 10)
        ("marmalade" . 5)))
(package-initialize)
(setq package-enable-at-startup nil)

;; define load paths
(setq default-directory (substitute-in-file-name "$HOME/"))
(defconst elpa-dir (substitute-in-file-name "$HOME/.emacs.d/elpa"))
(defconst nonelpa-dir (substitute-in-file-name "$HOME/.emacs.d/nonelpa"))
(defconst local-dir (substitute-in-file-name "$HOME/.emacs.d/config"))
(add-to-list 'load-path elpa-dir)
(add-to-list 'load-path nonelpa-dir)
(add-to-list 'load-path local-dir)

(load "kbd")
(require 'utils)

(define-coding-system-alias 'UTF-8 'utf-8)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; make scoll smooth
(setq scroll-step 1
	  scroll-margin 10
	  scroll-conservatively 10000)

;; making buffers with the same name distinguishable
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; line numbers
(global-linum-mode t)
;; (setq linum-format "%4d| ") ;; linum format

;; tab indent
;; (setq python-indent-offset 4)

;; no back-up or auto-save files
(setq-default make-backup-files nil ;; no backup file
			  backup-inhibited t ;; no backup
			  auto-save-mode nil ;; no auto save
			  auto-save-default nil) ;; no #file#

;; split windows vertically
(setq split-height-threshold nil
	  split-width-threshold 0)

;; wrap word
(toggle-word-wrap)

;; remember last position
(save-place-mode t)

;; when opening a help window, focus to it
(setq help-window-select t)

;; load customize variables
(setq custom-file (concat local-dir "/custom.el"))
(add-hook 'emacs-startup-hook
		  (lambda ()
			(load custom-file)
			(hyesun/sort-package-list)))

;; sort package-selected-list
(hyesun/sort-package-list)
(add-hook 'kill-emacs-hook 'hyesun/sort-package-list)

;; extra config
(load (concat local-dir "/extra-config.el"))
