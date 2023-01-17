;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(require 'utils)
(require 'init-modeline)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
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
(setq
  scroll-step
  1
  scroll-margin 10
  scroll-conservatively 10000)

;; making buffers with the same name distinguishable
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; line numbers
;; (setq display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode)

;; no back-up or auto-save files
(setq-default
  make-backup-files
  nil ;; no backup file
  backup-inhibited t ;; no backup
  auto-save-mode nil ;; no auto save
  auto-save-default nil) ;; no #file#
(setq auto-save-list-file-prefix nil)

;; split windows vertically
(setq
  split-height-threshold
  nil
  split-width-threshold 0)

;; wrap word
(toggle-word-wrap)

;; remember last position
(save-place-mode t)

;; when opening a help window, focus to it
(setq help-window-select t)

;; Don't prompt for large file
(setq large-file-warning-threshold nil)

;; gc threshold
(setq gc-cons-threshold most-positive-fixnum)

;; revert buffer when file changes
(global-auto-revert-mode)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; magit
(setq magit-ediff-dwim-show-on-hunks t)
(defun refresh-vc-state (&rest r)
  ;; (message "refresh vc state for %S" (current-buffer))
  (vc-refresh-state))
(advice-add 'magit-checkout-revision
  :after
  'refresh-vc-state
  '((name . "magit-refresh-on-checkout-revision")))
(advice-add 'magit-branch-create
  :after
  'refresh-vc-state
  '((name . "magit-refresh-on-branch-create")))
(advice-add 'magit-branch-and-checkout
  :after
  'refresh-vc-state
  '((name . "magit-refresh-on-checkout-and-branch")))
(advice-add 'magit-branch-or-checkout
  :after
  'refresh-vc-state
  '((name . "magit-refresh-on-branch-or-checkout")))
(setq magit-display-buffer-function
  'magit-display-buffer-same-window-except-diff-v1)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq vc-follow-symlinks t)
;; (xterm-mouse-mode t)

(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab)

(setq native-comp-async-report-warnings-errors nil)

(require 'tramp)
;; (setq tramp-debug-buffer t)
;; (setq tramp-verbose 3)
;; (setq tramp-default-method "sshx")
;; (tramp-change-syntax 'simplified)
(add-to-list 'tramp-connection-properties
  `(,(regexp-quote "/sshx:sg:") "remote-shell" "/bin/bash"))
(setq tramp-remote-path '(tramp-own-remote-path))
;; (setq vc-ignore-dir-regexp
;;   (format
;;     "\\(%s\\)\\|\\(%s\\)"
;;     vc-ignore-dir-regexp
;;     tramp-file-name-regexp))
(setq vc-handled-backends '(Git))
(remove-hook 'tramp-cleanup-connection-hook #'tramp-recentf-cleanup)
(remove-hook
  'tramp-cleanup-all-connections-hook
  #'tramp-recentf-cleanup-all)
;; set to nil if you use the ProxyCommand or ProxyJump options
(setq tramp-use-ssh-controlmaster-options nil)
(setq remote-file-name-inhibit-cache 86400)

;; load customize variables
(setq custom-file (concat emacs-dir "/custom.el"))
;; (load custom-file)
;; (add-hook 'kill-emacs-hook
;;           (lambda ()
;;             (custom-set-variables `(package-selected-packages ,(hs/sort-package-list)))
;;             (custom-save-all)))

(provide 'builtin)
