;; -*- mode: emacs-lisp -*-

;; --------------------------------------------------------------------------------------------------------------
;; evil
;; this line should be placed before `(require 'evil)'
(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-C-i-jump nil)
(require 'evil)
(evil-mode t)
(evil-set-initial-state 'package-menu-mode 'motion)
(defalias 'evil-insert-state 'evil-emacs-state)

;; (defun hyesun//evil-insert-to-emacs-state (evil-insert-state-func &rest args)
;;   (evil-emacs-state))
;; (advice-add 'evil-insert-state :around 'hyesun//evil-insert-to-emacs-state)

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
      evil-emacs-state-cursor '(bar))

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

;; --------------------------------------------------------------------------------------------------------------
;; key bindings
(defvar spc-leader-map (make-sparse-keymap))

;; emacs state
(define-key evil-emacs-state-map [escape] 'evil-force-normal-state)

;; normal state
(define-key evil-normal-state-map (kbd "SPC") spc-leader-map)
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

;; motion state
(define-key evil-motion-state-map (kbd "SPC") spc-leader-map)
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
(define-key evil-visual-state-map (kbd "SPC") spc-leader-map)
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

;; evil-mc
(global-evil-mc-mode t)

(defun hyesun/evil-mc-make-cursor-here ()
  (interactive)
  (evil-mc-make-cursor-here)
  (evil-mc-pause-cursors))

(define-key spc-leader-map "cm" 'hyesun/evil-mc-make-cursor-here)
(define-key spc-leader-map "cn" 'evil-mc-make-and-goto-next-match)

;; file
(define-key spc-leader-map "fl" 'load-file)
(define-key spc-leader-map "fp" 'hyesun/show-file-path)
(define-key spc-leader-map "fs" 'save-buffer)
(define-key spc-leader-map "fw" 'write-file)

;; line
(define-key spc-leader-map "ld" 'hyesun/kill-stripped-line)
(define-key spc-leader-map "lk" 'hyesun/backward-kill-line)
(define-key spc-leader-map "ll" 'hyesun/select-stripped-line)
(define-key spc-leader-map "lm" 'set-mark-command)
(define-key spc-leader-map "lw" 'hyesun/move-beginning-of-first-word)
(define-key spc-leader-map "l;" 'hyesun/comment-line)
(define-key spc-leader-map "l/" 'hyesun/uncomment-line)

;; region
(define-key spc-leader-map "r;" 'comment-region)
(define-key spc-leader-map "r/" 'uncomment-region)

;; buffer
(define-key spc-leader-map "bb" 'switch-to-buffer)
(define-key spc-leader-map "bd" 'kill-this-buffer)
(define-key spc-leader-map "bs" 'replace-string)
(define-key spc-leader-map "ba" 'mark-whole-buffer)
(define-key spc-leader-map "bD" 'hyesun/kill-user-buffers)

;; window
(define-key spc-leader-map "wd" 'delete-window)
(define-key spc-leader-map "wD" 'delete-other-windows)
(define-key spc-leader-map "wh" 'evil-window-left)
(define-key spc-leader-map "wl" 'evil-window-right)
(define-key spc-leader-map "w/" 'split-window-right)

;; others
(define-key spc-leader-map "hdk" 'describe-key)
(define-key spc-leader-map "hdf" 'describe-function)
(define-key spc-leader-map "qq" 'save-buffers-kill-terminal)
