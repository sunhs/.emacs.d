;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-
;; This is the entry point of init code.
;; The code included here is about built-in config.
;; `utils` and `kbd` are both about built-in settings.
;; `extra-config` is about system-independent package-specific config.
;; `local-config-[OS]` is about system-specific config.

;; define load paths
;; (setq default-directory (substitute-in-file-name "$HOME/"))
(defconst emacs-dir (substitute-in-file-name "$HOME/.config/emacs"))
(defconst config-dir (concat emacs-dir "/config"))
(defconst nonelpa-dir (concat emacs-dir "/nonelpa"))
(add-to-list 'load-path config-dir)
(add-to-list 'load-path nonelpa-dir)

;; elpa packages
(require 'package)
(let ((emacs-china-dir
       (concat emacs-dir "/emacs-china")))
  (if (file-exists-p emacs-china-dir)
      (setq package-archives `(("melpa" . ,(concat emacs-china-dir "/melpa"))
                               ("melpa-stable" . ,(concat emacs-china-dir "/melpa-stable"))
                               ("marmalade" . ,(concat emacs-china-dir "/marmalade"))
                               ("gnu" . ,(concat emacs-china-dir "/gnu"))))
    
    (setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")
                             ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                             ("marmalade" . "http://elpa.emacs-china.org/marmalade/")
                             ("gnu" . "http://elpa.emacs-china.org/gnu/")))))

(setq package-archive-priorities
      '(("melpa" . 20)
        ("melpa-stable" . 15)
        ("gnu" . 10)
        ("marmalade" . 5)))
(package-initialize)

(load "kbd")
(require 'utils)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; make utf-8 the default coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; don't use tab
(setq-default indent-tabs-mode nil)

;; make scoll smooth
(setq scroll-step 1
      scroll-margin 10
      scroll-conservatively 10000)

;; making buffers with the same name distinguishable
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; line numbers
(global-display-line-numbers-mode)

;; no back-up or auto-save files
(setq-default make-backup-files nil ;; no backup file
              backup-inhibited t ;; no backup
              auto-save-mode nil ;; no auto save
              auto-save-default nil) ;; no #file#
(setq auto-save-list-file-prefix nil)

;; split windows vertically
(setq split-height-threshold nil
      split-width-threshold 0)

;; wrap word
(toggle-word-wrap)

;; remember last position
(save-place-mode t)

;; when opening a help window, focus to it
(setq help-window-select t)

;; Don't prompt for large file
(setq large-file-warning-threshold nil)

;; mode-line
(setq-default mode-line-format
              (list
               "  "
               "%&"
               mode-line-buffer-identification
               '(vc-mode vc-mode)
               "    "
               mode-line-modes
               "    "
               mode-line-misc-info))

;; gc threshold
(setq gc-cons-threshold most-positive-fixnum)

;; revert buffer when file changes
(global-auto-revert-mode)

;; load customize variables
(setq custom-file (concat config-dir "/custom.el"))
(load custom-file)
;; (add-hook 'kill-emacs-hook
;;           (lambda ()
;;             (custom-set-variables `(package-selected-packages ,(hs/sort-package-list)))
;;             (custom-save-all)))

;; extra config
(load (concat config-dir "/extra-config.el"))
