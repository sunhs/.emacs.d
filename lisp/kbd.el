;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-
;; kbds for built-in functionality.
;; Note that if a kbd is package related
;; then you should place that kbd near its package config.

(require 'utils)

(defvar hs-leader-map (make-sparse-keymap))
(global-set-key (kbd "C-l") hs-leader-map)
;; (global-set-key (kbd "M-SPC") hs-leader-map)
(global-set-key (kbd "M-L") 'downcase-word)
(global-set-key (kbd "M-U") 'upcase-word)
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "C-v")
  (lambda () (interactive) (next-line (/ (window-height) 2))))
(global-set-key (kbd "M-v")
  (lambda () (interactive) (previous-line (/ (window-height) 2))))
(global-set-key (kbd "M-{") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "M-}") 'indent-rigidly-right-to-tab-stop)

;; project
(define-key hs-leader-map "p" project-prefix-map)
(define-key project-prefix-map "f" 'project-find-file)
(define-key project-prefix-map "d" 'project-find-dir)
(define-key project-prefix-map "p" 'project-switch-project)
(define-key project-prefix-map "b" 'project-switch-to-buffer)
(define-key project-prefix-map "g" 'project-find-regexp)

;; file
(define-key hs-leader-map "fl"
  #'(lambda () (interactive) (load-file (buffer-file-name))))
(define-key hs-leader-map "fp" 'hs/show-file-path)
(define-key hs-leader-map "fs" 'save-buffer)
(define-key hs-leader-map "fw" 'write-file)

;; line
(define-key hs-leader-map "ld" 'hs/kill-stripped-line)
(define-key hs-leader-map "lu" 'hs/backward-kill-line)
(define-key hs-leader-map "ll" 'hs/select-stripped-line)
(define-key hs-leader-map "lw" 'hs/move-beginning-of-first-word)
(define-key hs-leader-map "lx" 'hs/kill-whole-line)
;; this is for TUI emacs, pressing C-/ sends C-_
(global-set-key (kbd "C-_") 'hs/comment-uncomment-region)
;; this is for GUI emacs, pressing C-/ does exactly what I mean
(global-set-key (kbd "C-/") 'hs/comment-uncomment-region)
(define-key hs-leader-map "lt" 'hs-cmd/recenter-top)
(global-set-key (kbd "ESC <up>") 'hs/move-region-lines-up)
(global-set-key (kbd "ESC <down>") 'hs/move-region-lines-down)
(define-key hs-leader-map "lo" 'hs-cmd/new-line-bellow)
(define-key hs-leader-map "lO" 'hs-cmd/new-line-above)
;; duplicate line below
(define-key hs-leader-map "ly"
  #'
  (lambda ()
    (interactive)
    (beginning-of-line)
    (push-mark)
    (activate-mark)
    (end-of-line)
    (kill-ring-save (mark) (point))
    (end-of-line)
    (newline)
    (yank)))
;; duplicate line above
(define-key hs-leader-map "lY"
  #'
  (lambda ()
    (interactive)
    (hs/select-stripped-line)
    (beginning-of-line)
    (push-mark)
    (activate-mark)
    (end-of-line)
    (kill-ring-save (mark) (point))
    (previous-line)
    (end-of-line)
    (newline)
    (yank)))
(global-set-key (kbd "C-a") 'hs/smart-beginning-of-line)

;; region
(define-key hs-leader-map "r;" 'comment-region)
(define-key hs-leader-map "r/" 'uncomment-region)

;; buffer
(define-key hs-leader-map "bb" 'switch-to-buffer)
(define-key hs-leader-map "bd" 'kill-this-buffer)
(define-key hs-leader-map "bs" 'replace-string)
(define-key hs-leader-map "ba" 'mark-whole-buffer)
(define-key hs-leader-map "bD" 'hs/kill-user-buffers)
(define-key hs-leader-map "bn" 'hs/show-buffer-name)
(define-key hs-leader-map "gg" 'beginning-of-buffer)
(define-key hs-leader-map "G" 'end-of-buffer)

;; window
(global-set-key (kbd "M-i") 'windmove-up)
(global-set-key (kbd "M-k") 'windmove-down)
(global-set-key (kbd "M-j") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(define-key hs-leader-map "wa" 'ace-window)
(define-key hs-leader-map "wd" 'delete-window)
(define-key hs-leader-map "wD" 'delete-other-windows)
(define-key hs-leader-map "we" 'enlarge-window-horizontally)
(define-key hs-leader-map "ws" 'shrink-window-horizontally)
(define-key hs-leader-map "w/"
  (lambda () (interactive) (split-window-right) (windmove-right)))
(define-key hs-leader-map "w-"
  (lambda () (interactive) (split-window-below) (windmove-down)))
(define-key hs-leader-map "w="
  (lambda () (interactive) (balance-windows)))

;; avy
(defvar hs-avy-map (make-sparse-keymap))
(global-set-key (kbd "M-a") hs-avy-map)
(define-key hs-avy-map (kbd "c") 'avy-goto-char)
(define-key hs-avy-map (kbd "a") 'avy-goto-char-2)
(define-key hs-avy-map (kbd "l") 'avy-goto-end-of-line)
(define-key hs-avy-map (kbd "w") 'avy-goto-word-1)
(define-key hs-avy-map (kbd "s") 'avy-goto-char-timer)
(define-key hs-avy-map (kbd "r") 'avy-resume)

;; help
(define-key hs-leader-map "hk" 'describe-key)
(define-key hs-leader-map "hf" 'describe-function)
(define-key hs-leader-map "hv" 'describe-variable)

;; others
(define-key hs-leader-map "mm" 'set-mark-command)
(global-set-key (kbd "M-m") 'set-mark-command)
(define-key hs-leader-map "qq"
  #'
  (lambda ()
    (interactive)
    (if (delete-frame-enabled-p)
      (delete-frame)
      (save-buffers-kill-terminal))))
(global-set-key (kbd "C-j")
  (lambda () (interactive) (move-end-of-line 1) (newline-and-indent)))

(provide 'kbd)
