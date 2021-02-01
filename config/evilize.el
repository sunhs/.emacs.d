;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-
(setq hs/use-evil-p t)

;; --------------------------------------------------------------------------------------------------------------
;; evil
;; this line should be placed before `(require 'evil)'
(setq-default evil-want-C-u-scroll nil)
(setq-default evil-want-C-i-jump nil)
(require 'evil)
(evil-mode t)
(evil-set-initial-state 'package-menu-mode 'motion)
(defalias 'evil-insert-state 'evil-emacs-state)

;; (defun hs//evil-insert-to-emacs-state (evil-insert-state-func &rest args)
;;   (evil-emacs-state))
;; (advice-add 'evil-insert-state :around 'hs//evil-insert-to-emacs-state)

;; Override stock evil function `evil-emacs-state-p'
;; Since emacs-state is blocked in evil-mc and evil-esc
(defun evil-emacs-state-p (&optional state)
  "Whether the current state is insert."
  (and evil-local-mode
       (memq (or state evil-state) '())))

;; --------------------------------------------------------------------------------------------------------------
;; evil-terminal-cursor-changer
;; These lines are vital for evil-terminal-cursor-changer to work normally,
;; since the default normal-state-cursor is `t`.
(setq evil-normal-state-cursor '(box)
      evil-emacs-state-cursor '(box))
      ;; evil-emacs-state-cursor '(bar))

(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

;; --------------------------------------------------------------------------------------------------------------
;; evil-escape
(require 'evil-escape)
(evil-escape-mode t)
(setq-default evil-escape-delay 0.2)

;; --------------------------------------------------------------------------------------------------------------
;; evil-org
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; evil-magit
;; (use-package evil-magit
;;   :config
;;   (evil-define-key evil-magit-state magit-mode-map (kbd "SPC") hs-leader-map))

;; --------------------------------------------------------------------------------------------------------------
;; key bindings
;; (defvar hs-leader-map (make-sparse-keymap))

;; emacs state
(define-key evil-emacs-state-map [escape] 'evil-force-normal-state)

;; normal state
(define-key evil-normal-state-map (kbd "SPC") hs-leader-map)
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "$" 'evil-end-of-visual-line)
(define-key evil-normal-state-map "^" 'evil-first-non-blank-of-visual-line)
(define-key evil-normal-state-map [tab] 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "J") 'evil-goto-line)
(define-key evil-normal-state-map (kbd "K") 'evil-goto-first-line)
(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank-of-visual-line)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-visual-line)
(define-key evil-normal-state-map (kbd "[") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "]") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "M-,") 'xref-pop-marker-stack)

;; motion state
(define-key evil-motion-state-map (kbd "SPC") hs-leader-map)
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
(define-key evil-motion-state-map "$" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map (kbd "J") 'evil-goto-line)
(define-key evil-motion-state-map (kbd "K") 'evil-goto-first-line)
(define-key evil-motion-state-map (kbd "H") 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map (kbd "L") 'evil-end-of-visual-line)
(define-key evil-motion-state-map (kbd "[") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "]") 'evil-scroll-down)

;; visual state
(define-key evil-visual-state-map (kbd "SPC") hs-leader-map)
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)
(define-key evil-visual-state-map "$" 'evil-end-of-visual-line)
(define-key evil-visual-state-map "^" 'evil-first-non-blank-of-visual-line)
(define-key evil-visual-state-map (kbd "J") 'evil-goto-line)
(define-key evil-visual-state-map (kbd "K") 'evil-goto-first-line)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank-of-visual-line)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-visual-line)
(define-key evil-visual-state-map (kbd "[") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "]") 'evil-scroll-down)
;; (define-key evil-visual-state-map ";" 'comment-region)
;; (define-key evil-visual-state-map "/" 'uncomment-region)

;; evil-mc
(global-evil-mc-mode t)

(defun hs/evil-mc-make-cursor-here ()
  (interactive)
  (evil-mc-make-cursor-here)
  (evil-mc-pause-cursors))

(define-key hs-leader-map "cm" 'hs/evil-mc-make-cursor-here)
(define-key hs-leader-map "cn" 'evil-mc-make-and-goto-next-match)

;; file
;; (define-key hs-leader-map "fl" 'load-file)
;; (define-key hs-leader-map "fp" 'hs/show-file-path)
(define-key hs-leader-map "fs" 'save-buffer)
(define-key hs-leader-map "fw" 'write-file)

;; line
;; (define-key hs-leader-map "ld" 'hs/kill-stripped-line)
;; (define-key hs-leader-map "lu" 'hs/backward-kill-line)
;; (define-key hs-leader-map "ll" 'hs/select-stripped-line)
;; (define-key hs-leader-map "lm" 'set-mark-command)
;; (define-key hs-leader-map "lw" 'hs/move-beginning-of-first-word)
;; (define-key hs-leader-map "lx" 'hs/kill-whole-line)
;; (define-key hs-leader-map "l;" 'hs/comment-line)
;; (define-key hs-leader-map "l/" 'hs/uncomment-line)

;; region
;; (define-key hs-leader-map "r;" 'comment-region)
;; (define-key hs-leader-map "r/" 'uncomment-region)

;; buffer
;; (define-key hs-leader-map "bb" 'switch-to-buffer)
;; (define-key hs-leader-map "bd" 'kill-this-buffer)
;; (define-key hs-leader-map "bs" 'replace-string)
;; (define-key hs-leader-map "ba" 'mark-whole-buffer)
;; (define-key hs-leader-map "bD" 'hs/kill-user-buffers)
;; (define-key hs-leader-map "bn" 'hs/show-buffer-name)

;; window
;; (define-key hs-leader-map "wa" 'ace-window)
;; (define-key hs-leader-map "wd" 'delete-window)
;; (define-key hs-leader-map "wD" 'delete-other-windows)
;; (define-key hs-leader-map "wh" 'evil-window-left)
;; (define-key hs-leader-map "wl" 'evil-window-right)
;; (define-key hs-leader-map "we" 'enlarge-window-horizontally)
;; (define-key hs-leader-map "ws" 'shrink-window-horizontally)
;; (define-key hs-leader-map "w/"
;;   (lambda ()
;;     (interactive)
;;     (split-window-right)
;;     (windmove-right)))

;; others
(define-key hs-leader-map "hdk" 'describe-key)
(define-key hs-leader-map "hdf" 'describe-function)
(define-key hs-leader-map "qq" 'save-buffers-kill-terminal)
