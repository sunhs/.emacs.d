;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-
;; kbds for built-in functionality.
;; Note that if a kbd is package related
;; then you should place that kbd near its package config.

(require 'utils)

(defvar hs-leader-map (make-sparse-keymap))
(global-set-key (kbd "C-s") hs-leader-map)
(global-set-key (kbd "M-L") 'downcase-word)
(global-set-key (kbd "M-U") 'upcase-word)
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-l"))

;; file
(define-key hs-leader-map "fl" 'load-file)
(define-key hs-leader-map "fp" 'hs/show-file-path)

;; format
(define-key hs-leader-map "fb"
  (lambda ()
    (interactive)
    (if (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
        (clang-format-buffer)
      (lsp-format-buffer))))

(define-key hs-leader-map "fr"
  (lambda ()
    (interactive)
    (if (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
        (clang-format-region (region-beginning) (region-end))
        ;; (message (number-to-string (region-beginning)))
      (lsp-format-region (region-beginning) (region-end)))))

;; line
(define-key hs-leader-map "ld" 'hs/kill-stripped-line)
(define-key hs-leader-map "lu" 'hs/backward-kill-line)
(define-key hs-leader-map "ll" 'hs/select-stripped-line)
(define-key hs-leader-map "lm" 'set-mark-command)
(define-key hs-leader-map "lw" 'hs/move-beginning-of-first-word)
(define-key hs-leader-map "lx" 'hs/kill-whole-line)
(define-key hs-leader-map "l;" 'hs/comment-line)
(define-key hs-leader-map "l/" 'hs/uncomment-line)
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
(define-key hs-leader-map "wa" 'ace-window)
(define-key hs-leader-map "wd" 'delete-window)
(define-key hs-leader-map "wD" 'delete-other-windows)
(define-key hs-leader-map "wh" 'windmove-left)
(define-key hs-leader-map "wl" 'windmove-right)
(define-key hs-leader-map "wj" 'windmove-down)
(define-key hs-leader-map "wk" 'windmove-up)
(define-key hs-leader-map "we" 'enlarge-window-horizontally)
(define-key hs-leader-map "ws" 'shrink-window-horizontally)
;(global-set-key (kbd "C-c C-x <up>") 'enlarge-window-horizontally)
;(global-set-key (kbd "C-c C-x <down>") 'shrink-window-horizontally)
(define-key hs-leader-map "w/"
  (lambda ()
    (interactive)
    (split-window-right)
    (windmove-right)))
(define-key hs-leader-map "w-"
  (lambda ()
    (interactive)
    (split-window-below)
    (windmove-down)))

;; others
(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (move-end-of-line 1)
                  (newline-and-indent)))
