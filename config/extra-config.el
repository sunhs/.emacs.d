;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

;; --------------------------------------------------------------------------------------------------------------
;; evil
(load "evilize")

;; --------------------------------------------------------------------------------------------------------------
;; intellisense
(load "intellisense")

;; --------------------------------------------------------------------------------------------------------------
;; smartparens
(use-package smartparens
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)
  (define-key spc-leader-map (kbd "sph") 'sp-beginning-of-sexp)
  (define-key spc-leader-map (kbd "spl") 'sp-end-of-sexp)
  (define-key spc-leader-map (kbd "spbu") 'sp-backward-up-sexp)
  (define-key spc-leader-map (kbd "spbd") 'sp-backward-down-sexp)
  (define-key spc-leader-map (kbd "spfu") 'sp-up-sexp)
  (define-key spc-leader-map (kbd "spfd") 'sp-down-sexp)
  (sp-pair "\\\\(" nil :unless '(sp-point-before-word-p))
  (sp-pair "\\{" nil :unless '(sp-point-before-word-p))
  (sp-pair "\\(" nil :unless '(sp-point-before-word-p))
  (sp-pair "\\\"" nil :unless '(sp-point-before-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p))
  (sp-pair "'" nil :unless '(sp-point-before-word-p))
  (sp-pair "`" nil :unless '(sp-point-before-word-p))
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p)))

;; --------------------------------------------------------------------------------------------------------------
;; ivy / swiper / counsel
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-height 15
        ivy-extra-directories nil
        ivy-count-format "%d/%d")
  (setq ivy-ignore-buffers (append '("*Help*") ivy-ignore-buffers)))

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  (define-key spc-leader-map "ss" 'swiper))

(use-package counsel
  :config
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c C-x y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key spc-leader-map (kbd "SPC") 'counsel-M-x)
  (define-key spc-leader-map (kbd "y") 'counsel-yank-pop)
  (define-key spc-leader-map "ff" 'counsel-find-file))

;; --------------------------------------------------------------------------------------------------------------
;; projectile
;; (use-package projectile
;;   :config
;;   (projectile-mode t))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (define-key spc-leader-map "p" 'projectile-command-map)
  (define-key projectile-command-map "f" 'projectile-find-file)
  (define-key projectile-command-map "d" 'projectile-find-dir)
  (define-key projectile-command-map "p" 'projectile-switch-project)
  (define-key projectile-command-map "b" 'projectile-switch-to-buffer)
  (define-key projectile-command-map "g" 'projectile-ripgrep))

;; --------------------------------------------------------------------------------------------------------------
;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "M-.") 'undo-tree-undo)
  (global-set-key (kbd "M-,") 'undo-tree-redo)
  (define-key evil-normal-state-map "r" 'undo-tree-redo))

;; --------------------------------------------------------------------------------------------------------------
;; point-undo
(use-package point-undo
  :config
  (global-set-key (kbd "M-u") 'point-undo)
  (global-set-key (kbd "M-r") 'point-redo))

;; --------------------------------------------------------------------------------------------------------------
;; rainbow-delimiters-mode
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((emacs-lisp-mode
          python-mode
          c++-mode
          c-mode) . rainbow-delimiters-mode))

;; --------------------------------------------------------------------------------------------------------------
;; spaceline
(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator nil
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

;; --------------------------------------------------------------------------------------------------------------
;; company
(use-package company
  :config
  (global-company-mode t)
  (setq completion-styles '(basic substring partial-completion emacs22)
        company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-search-regexp-function 'company-search-words-in-any-order-regexp
        company-selection-wrap-around t)

  ;; turn off and on fci-mode during completion to avoid weird behavior
  ;; (defvar-local company-fci-mode-on-p nil)

  ;; (defun company-turn-off-fci (&rest ignore)
  ;;   (when (boundp 'fci-mode)
  ;;     (setq company-fci-mode-on-p fci-mode)
  ;;     (when company-fci-mode-on-p (fci-mode -1))))

  ;; (defun company-maybe-turn-on-fci (&rest ignore)
  ;;   (when company-fci-mode-on-p (fci-mode 1)))

  ;; (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  ;; (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  ;; (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  ;; (define-key company-active-map (kbd "TAB") 'company-complete)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  (company-statistics-mode t))
  ;; (company-quickhelp-mode t))

;; --------------------------------------------------------------------------------------------------------------
;; yasnippet
(use-package yasnippet
  :config
  ;; (yas-global-mode t)
  (setq yas-snippet-dirs (list (concat emacs-dir "/snippets"))))

;; --------------------------------------------------------------------------------------------------------------
;; blacken
(use-package blacken
  :config
  (setq blacken-line-length 79))

;; --------------------------------------------------------------------------------------------------------------
;; config relating to platforms
(add-to-list 'load-path (concat config-dir "/platform"))
(load "local-config-loader")

;; --------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------- Deprecated -------------------------------------------------
;; --------------------------------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------------------------------
;; fill column indicator
;; (defun my-fci-conf ()
;;   (require 'fill-column-indicator)
;;   (setq fci-rule-column 80
;;         fci-rule-color "#40e0d0"
;;         fci-rule-use-dashes t)
;;   (fci-mode 1))
;; (add-hook 'python-mode-hook 'my-fci-conf)
;; (add-hook 'c-mode-hook 'my-fci-conf)
;; (add-hook 'c++-mode-hook 'my-fci-conf)

;; --------------------------------------------------------------------------------------------------------------
;; smooth-scrolling
;; (require 'smooth-scrolling)

;; --------------------------------------------------------------------------------------------------------------
;; inertial-scroll
;; (require 'inertial-scroll)
;; (inertias-global-minor-mode 1)
;; (setq inertias-initial-velocity-wheel 1)

;; --------------------------------------------------------------------------------------------------------------
;; neotree
;; (require 'neotree)
;; (global-set-key (kbd "C-c n") 'neotree-toggle)

;; --------------------------------------------------------------------------------------------------------------
;; sr-speedbar
;; (setq sr-speedbar-right-side nil
;;       speedbar-show-unknown-files t
;;       sr-speedbar-auto-refresh nil)

;; (defun focus-speedbar-window ()
;;   (interactive)
;;   (lexical-let ((found nil))
;;     (dolist (w (window-list))
;;       (if (string= (buffer-name (window-buffer w))
;;                    "*SPEEDBAR*")
;;           (progn
;;             (select-window w)
;;             (setq found t))))
;;     (unless found
;;       (sr-speedbar-open))))

;; (global-set-key (kbd "C-c s") 'focus-speedbar-window)

;; --------------------------------------------------------------------------------------------------------------
;; magit
;; (define-key spc-leader-map (kbd "ms") 'magit-status)
