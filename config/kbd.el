;; -*- mode: emacs-lisp -*-
;; kbds for built-in functionality.
;; Note that if a kbd is package related
;; then you should place that kbd near its package config.

(require 'utils)

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c C-x m") 'set-mark-command)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c /") 'uncomment-region)
(global-set-key (kbd "C-c C-f") 'load-file)
(global-set-key (kbd "C-c C-x s") 'replace-string)
(global-set-key (kbd "C-c C-x <up>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-x <down>") 'shrink-window-horizontally)

(global-set-key (kbd "C-a") 'hyesun/smart-beginning-of-line)
(global-set-key (kbd "C-c C-x ;") 'hyesun/comment-line)
(global-set-key (kbd "C-c C-x /") 'hyesun/uncomment-line)
(global-set-key (kbd "C-c C-x f") 'hyesun/show-file-path)
(global-set-key (kbd "C-c C-x b") 'hyesun/show-buffer-name)
(global-set-key (kbd "C-c C-x k") 'hyesun/backward-kill-line)
(global-set-key (kbd "C-c C-x w") 'hyesun/move-beginning-of-first-word)
(global-set-key (kbd "C-c C-x l") 'hyesun/select-stripped-line)
(global-set-key (kbd "C-c C-x d") 'hyesun/kill-stripped-line)
(global-set-key (kbd "C-c C-x x") 'hyesun/kill-whole-line)
