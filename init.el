;; -*- mode: emacs-lisp -*-
;; This is the entry point of init code.
;; The code included here is about built-in config.
;; `utils` and `kbd` are both about built-in settings.
;; `extra-config` is about system-independent package-specific config.
;; `local-config-[OS]` is about system-specific config.

;; define load paths
(setq default-directory (substitute-in-file-name "$HOME/"))
(defconst emacs-dir (substitute-in-file-name "$HOME/.emacs.d"))
(defconst config-dir (substitute-in-file-name "$HOME/.emacs.d/config"))
(add-to-list 'load-path config-dir)
(add-to-list 'load-path (concat emacs-dir "/nonelpa"))

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
(setq package-enable-at-startup nil)

(setenv "no_proxy" "127.0.0.1")
(setenv "NO_PROXY" "127.0.0.1")

(load "kbd")
(require 'utils)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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
(global-linum-mode t)
(setq linum-format "%4d| ") ;; linum format

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
(setq custom-file (concat config-dir "/custom.el"))
(add-hook 'emacs-startup-hook
          (lambda ()
            (load custom-file)
            (hyesun/sort-package-list)))

;; sort package-selected-list
(hyesun/sort-package-list)
(add-hook 'kill-emacs-hook 'hyesun/sort-package-list)

;; extra config
(load (concat config-dir "/extra-config.el"))
