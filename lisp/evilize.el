;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

;; ---------------------------------------------------- evil ----------------------------------------------------
;; this line should be placed before `(require 'evil)'
(setq-default evil-want-C-u-scroll nil)
(setq-default evil-want-C-i-jump nil)
(require 'evil)
(evil-mode t)
(evil-set-initial-state 'package-menu-mode 'motion)
(dolist (mode '(org-mode))
  (evil-set-initial-state mode 'emacs))
(defalias 'evil-insert-state 'evil-emacs-state)
;; (defalias 'forward-evil-word 'forward-evil-symbol)

;; (defun hs//evil-insert-to-emacs-state (evil-insert-state-func &rest args)
;;   (evil-emacs-state))
;; (advice-add 'evil-insert-state :around 'hs//evil-insert-to-emacs-state)

;; Override stock evil function `evil-emacs-state-p'
;; Since emacs-state is blocked in evil-mc and evil-esc
(defun evil-emacs-state-p (&optional state)
  "Whether the current state is insert."
  (and evil-local-mode
       (memq (or state evil-state) '())))

;; ---------------------------------------- evil-terminal-cursor-changer ----------------------------------------
;; These lines are vital for evil-terminal-cursor-changer to work normally,
;; since the default normal-state-cursor is `t`.
(setq evil-normal-state-cursor '(box))
      ;; evil-emacs-state-cursor '(hbar))
      ;; evil-emacs-state-cursor '(bar))

(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  ;; (setq etcc-use-color t)
  (evil-terminal-cursor-changer-activate))
;; (add-hook 'evil-emacs-state-entry-hook #'(lambda ()
;;                                           (set-face-background 'mode-line "midnightblue")))
;; (add-hook 'evil-emacs-state-exit-hook #'(lambda ()
;;                                           (set-face-background 'mode-line nil)))

;; ------------------------------------------------- evil-escape ------------------------------------------------
;; evil-escape
(require 'evil-escape)
(evil-escape-mode t)
(setq-default evil-escape-delay 0.2)

;; -------------------------------------------------- evil-org --------------------------------------------------
;; evil-org
;; (use-package evil-org
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

;; ------------------------------------------------- evil-magit -------------------------------------------------
;; evil-magit
;; (use-package evil-magit
;;   :config
;;   (evil-define-key evil-magit-state magit-mode-map (kbd "SPC") hs-leader-map))

;; ----------------------------------------------- evil-surround ------------------------------------------------
;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; ------------------------------------------------ key bindings ------------------------------------------------

;; emacs state
(define-key evil-emacs-state-map [escape] 'evil-force-normal-state)

;; common for state maps
(defun define-key-for-evil-state-maps (list-state-map kbd func)
  (dolist (state-map list-state-map)
    (define-key (symbol-value state-map) kbd func)))

(setq common-evil-state-maps '(evil-normal-state-map evil-motion-state-map evil-visual-state-map))
(define-key-for-evil-state-maps common-evil-state-maps (kbd "SPC") hs-leader-map)
;; (define-key-for-evil-state-maps common-evil-state-maps "j" 'evil-next-visual-line)
;; (define-key-for-evil-state-maps common-evil-state-maps "k" 'evil-previous-visual-line)
;; (define-key-for-evil-state-maps common-evil-state-maps "$" 'evil-end-of-visual-line)
;; (define-key-for-evil-state-maps common-evil-state-maps "^" 'evil-first-non-blank-of-visual-line)
(define-key-for-evil-state-maps common-evil-state-maps "q" 'delete-window)
(define-key-for-evil-state-maps common-evil-state-maps [tab] 'indent-for-tab-command)
(define-key-for-evil-state-maps common-evil-state-maps (kbd "TAB") 'indent-for-tab-command)
;; (define-key-for-evil-state-maps common-evil-state-maps (kbd "H") 'evil-first-non-blank-of-visual-line)
(define-key-for-evil-state-maps common-evil-state-maps (kbd "H") 'evil-first-non-blank)
;; (define-key-for-evil-state-maps common-evil-state-maps (kbd "L") 'evil-end-of-visual-line)
(define-key-for-evil-state-maps common-evil-state-maps (kbd "L") 'evil-end-of-line)
(define-key-for-evil-state-maps common-evil-state-maps (kbd "M-k") '(lambda () (interactive) (evil-scroll-up 20)))
(define-key-for-evil-state-maps common-evil-state-maps (kbd "M-j") '(lambda () (interactive) (evil-scroll-down 20)))
(define-key-for-evil-state-maps common-evil-state-maps (kbd "[") '(lambda () (interactive) (evil-scroll-up 20)))
(define-key-for-evil-state-maps common-evil-state-maps (kbd "]") '(lambda () (interactive) (evil-scroll-down 20)))
(define-key-for-evil-state-maps common-evil-state-maps (kbd "{")
  '(lambda ()
     (interactive)
     (hs/jump-up-half)))
(define-key-for-evil-state-maps common-evil-state-maps (kbd "}")
  '(lambda ()
     (interactive)
     (hs/jump-down-half)))

;; normal state
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "M-,") 'xref-pop-marker-stack)

;; motion state

;; visual state

;; -------------------------------------------------- evil-mc ---------------------------------------------------
(global-evil-mc-mode t)

(defun hs/evil-mc-make-cursor-here ()
  (interactive)
  (evil-mc-make-cursor-here)
  (evil-mc-pause-cursors))

(define-key hs-leader-map "cm" 'hs/evil-mc-make-cursor-here)
(define-key hs-leader-map "cn" 'evil-mc-make-and-goto-next-match)

;; ------------------------------------------------- evil-snipe -------------------------------------------------
;; evil-snipe
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
(setq evil-snipe-scope 'whole-buffer
      evil-snipe-repeat-scope 'whole-buffer)

(provide 'evilize)
