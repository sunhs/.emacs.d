;; -*- mode: emacs-lisp -*-

(require 'utils)

;; --------------------------------------------------------------------------------------------------------------
;; evil
(load "evilize")

;; --------------------------------------------------------------------------------------------------------------
;; smartparens
(require 'smartparens)
(smartparens-global-mode)
(require 'smartparens-config)
(sp-local-pair '(emacs-lisp-mode-hook) "'" "'" :actions nil)
(sp-local-pair '(lisp-mode-hook) "'" "'" :actions nil)
(sp-local-pair '(lisp-interaction-mode-hook) "'" "'" :actions nil)

;; --------------------------------------------------------------------------------------------------------------
;; ivy-mode
(ivy-mode 1)
(setq ivy-initial-inputs-alist nil
      ivy-use-virtual-buffers t
      ivy-height 15
      ivy-extra-directories nil
      ivy-count-format "%d/%d")
(setq ivy-ignore-buffers (append '("*Help*") ivy-ignore-buffers))
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c C-x y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(define-key spc-leader-map (kbd "SPC") 'counsel-M-x)
(define-key spc-leader-map (kbd "y") 'counsel-yank-pop)
(define-key spc-leader-map "ff" 'counsel-find-file)

;; --------------------------------------------------------------------------------------------------------------
;; swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(define-key spc-leader-map "ss" 'swiper)

;; --------------------------------------------------------------------------------------------------------------
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "M-.") 'undo-tree-undo)
(global-set-key (kbd "M-,") 'undo-tree-redo)
(define-key evil-normal-state-map "r" 'undo-tree-redo)

;; --------------------------------------------------------------------------------------------------------------
;; fill column indicator
(defun my-fci-conf ()
  (require 'fill-column-indicator)
  (setq fci-rule-column 80
        fci-rule-color "#40e0d0"
        fci-rule-use-dashes t)
  (fci-mode 1))
(add-hook 'python-mode-hook 'my-fci-conf)
(add-hook 'c-mode-hook 'my-fci-conf)
(add-hook 'c++-mode-hook 'my-fci-conf)

;; --------------------------------------------------------------------------------------------------------------
;; rainbow-delimiters-mode
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; --------------------------------------------------------------------------------------------------------------
;; smooth-scrolling
(require 'smooth-scrolling)

;; --------------------------------------------------------------------------------------------------------------
;; inertial-scroll
(require 'inertial-scroll)
(inertias-global-minor-mode 1)
(setq inertias-initial-velocity-wheel 1)

;; --------------------------------------------------------------------------------------------------------------
;; neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; --------------------------------------------------------------------------------------------------------------
;; sr-speedbar
(setq sr-speedbar-right-side nil
      speedbar-show-unknown-files t
      sr-speedbar-auto-refresh nil)

(defun focus-speedbar-window ()
  (interactive)
  (lexical-let ((found nil))
    (dolist (w (window-list))
      (if (string= (buffer-name (window-buffer w))
                   "*SPEEDBAR*")
          (progn
            (select-window w)
            (setq found t))))
    (unless found
      (sr-speedbar-open))))

(global-set-key (kbd "C-c s") 'focus-speedbar-window)

;; --------------------------------------------------------------------------------------------------------------
;; web-mode
(add-to-list 'auto-mode-alist '("\\.\\(php\\|ejs\\|html\\|js\\)$" . web-mode))
(add-hook 'web-mode-hook '(lambda () (setq web-mode-code-indent-offset 2)))

;; --------------------------------------------------------------------------------------------------------------
;; spaceline
(require 'spaceline-config)
(setq powerline-default-separator nil
      spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(spaceline-spacemacs-theme)

;; --------------------------------------------------------------------------------------------------------------
;; company
(global-company-mode t)
(setq completion-styles '(basic substring partial-completion emacs22)
      company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-require-match nil)
(setq company-search-regexp-function 'company-search-words-in-any-order-regexp)

;; turn off and on fci-mode during completion to avoid weird behavior
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when company-fci-mode-on-p (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

(define-key company-active-map (kbd "TAB") 'company-complete)
(define-key company-active-map (kbd "<tab>") 'company-complete)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

(company-statistics-mode t)
(company-quickhelp-mode t)

;; --------------------------------------------------------------------------------------------------------------
;; yasnippet
(yas-global-mode t)
(setq yas-snippet-dirs (substitute-in-file-name "$HOME/.emacs.d/snippets"))

;; --------------------------------------------------------------------------------------------------------------
;; counsel-projectile
(use-package counsel-projectile
  :init
  (add-hook 'python-mode-hook 'counsel-projectile-mode)
  (add-hook 'c++-mode-hook 'counsel-projectile-mode)
  (add-hook 'c-mode-hook 'counsel-projectile-mode)
  (add-hook 'emacs-lisp-mode-hook 'counsel-projectile-mode)
  :config
  (define-key spc-leader-map (kbd "p") 'counsel-projectile-command-map))

;; --------------------------------------------------------------------------------------------------------------
;; magit
(define-key spc-leader-map (kbd "ms") 'magit-status)

;; --------------------------------------------------------------------------------------------------------------
;; config relating to platforms
(add-to-list 'load-path (concat config-dir "/platform"))
(load "local-config-loader")
