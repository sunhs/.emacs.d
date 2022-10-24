;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

;; ---------------------------------------------------- evil ----------------------------------------------------
(load "evilize")

;; -------------------------------------------------- killboar --------------------------------------------------
;; (load "killboard")

;; ---------------------------------------------------- dap -----------------------------------------------------
(load "dap")

;; --------------------------------------------- quelpa-use-package ---------------------------------------------
(require 'quelpa-use-package)
(setq quelpa-update-melpa-p nil)

;; ------------------------------------------------- smartparens ------------------------------------------------
(use-package smartparens
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)
  ;; (define-key-for-evil-state-maps common-evil-state-maps (kbd "(") 'sp-beginning-of-sexp)
  ;; (define-key-for-evil-state-maps common-evil-state-maps (kbd ")") 'sp-end-of-sexp)
  (define-key hs-leader-map "(u" 'sp-backward-up-sexp)
  (define-key hs-leader-map "(d" 'sp-backward-down-sexp)
  (define-key hs-leader-map "(fu" 'sp-up-sexp)
  (define-key hs-leader-map "(fd" 'sp-down-sexp)

  (defvar sp-mode-map (make-sparse-keymap))  
  (global-set-key (kbd "M-(") sp-mode-map)
  (define-key sp-mode-map (kbd "a") 'sp-beginning-of-sexp)
  (define-key sp-mode-map (kbd "e") 'sp-end-of-sexp)
  (define-key sp-mode-map (kbd "ua") 'sp-backward-up-sexp)
  (define-key sp-mode-map (kbd "da") 'sp-backward-down-sexp)
  (define-key sp-mode-map (kbd "ue") 'sp-up-sexp)
  (define-key sp-mode-map (kbd "de") 'sp-down-sexp)
  (setq prohibition-sym-list '("\\\\("
                               "\\{"
                               "\\("
                               "\\\""
                               "\""
                               "'"
                               "`"
                               "("
                               "["
                               "{"))
  (dolist (sym prohibition-sym-list)
    (sp-pair sym nil :unless '(sp-point-before-word-p
                               sp-point-before-same-p))))

;; --------------------------------------------------------------------------------------------------------------
;; <<<<<<<<<<<<<<O<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ivy / swiper / counsel >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; (use-package ivy
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-initial-inputs-alist nil
;;         ivy-use-virtual-buffers t
;;         ivy-height 15
;;         ivy-extra-directories nil
;;         ivy-count-format "%d/%d")
;;   (setq ivy-ignore-buffers (append '("*Help*") ivy-ignore-buffers)))

;; (use-package ivy-rich
;;   :ensure t
;;   :init (ivy-rich-mode 1))

;; (use-package swiper
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper)
;;   (global-set-key (kbd "C-r") 'swiper))
  

;; (use-package counsel
;;   :config
;;   (setq ivy-initial-inputs-alist nil)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-c C-x y") 'counsel-yank-pop)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (define-key hs-leader-map (kbd "SPC") 'counsel-M-x)
;;   ;; (define-key hs-leader-map (kbd "y") 'counsel-yank-pop)
;;   (define-key hs-leader-map "ff" 'counsel-find-file))

;; ------------------------------------------------- projectile -------------------------------------------------
(use-package projectile
  :config
  (projectile-mode t))

;; (use-package counsel-projectile
;;   :config
;;   (counsel-projectile-mode t)
;;   (define-key hs-leader-map "p" 'projectile-command-map)
;;   (define-key projectile-command-map "f" 'projectile-find-file)
;;   (define-key projectile-command-map "d" 'projectile-find-dir)
;;   (define-key projectile-command-map "p" 'projectile-switch-project)
;;   (define-key projectile-command-map "b" 'projectile-switch-to-buffer)
;;   (define-key projectile-command-map "g" 'projectile-ripgrep))

;; --------------------------------------------------------------------------------------------------------------
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< vertico / consult / orderless >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :config
  ;; define it here rather than evil, to better switch between ivy/counsel/swiper and vertico/consult
  (define-key hs-leader-map "ff" 'find-file)
  (define-key hs-leader-map (kbd "SPC") 'execute-extended-command)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :init (recentf-mode)
  :config
  (setq consult-project-function #'projectile-project-root)
  (global-set-key (kbd "C-s") 'consult-line)
  (define-key hs-leader-map (kbd "bb") 'consult-buffer)
  (define-key hs-leader-map "p" 'projectile-command-map)
  (define-key projectile-command-map "f" 'consult-projectile-find-file)
  (define-key projectile-command-map "d" 'consult-projectile-find-dir)
  (define-key projectile-command-map "p" 'consult-projectile-switch-project)
  (define-key projectile-command-map "b" 'consult-projectile-switch-to-buffer)
  (define-key projectile-command-map "g" 'consult-ripgrep))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)  ;; overridden in company
        ;; completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("M-o" . embark-act)         ;; pick some comfortable binding
   ;; ("M-d" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; --------------------------------------------------------------------------------------------------------------
;; treemacs
(use-package treemacs
  :commands treemacs
  :init
  (define-key hs-leader-map "wt" 'treemacs-select-window)
  :config
  (setq treemacs-follow-after-init t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))
  ;; (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

;; --------------------------------------------------------------------------------------------------------------
;; undo-tree
;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode)
;;   (global-set-key (kbd "M-u") 'undo-tree-undo)
;;   (global-set-key (kbd "M-r") 'undo-tree-redo)
;;   (hs/define-key-when-set hs/use-evil-p evil-normal-state-map "r" 'undo-tree-redo))

;; --------------------------------------------------------------------------------------------------------------
;; undo-fu
(use-package undo-fu
  :config
  ;; (global-undo-tree-mode -1)
  (global-set-key (kbd "M-u") 'undo-fu-only-undo)
  (global-set-key (kbd "M-r") 'undo-fu-only-redo)
  (hs/define-key-when-set hs/use-evil-p evil-normal-state-map "u" 'undo-fu-only-undo)
  (hs/define-key-when-set hs/use-evil-p evil-normal-state-map "r" 'undo-fu-only-redo))

;; --------------------------------------------------------------------------------------------------------------
;; point-undo
(use-package point-undo
  :config
  (global-set-key (kbd "M-;") 'point-undo)
  (global-set-key (kbd "M-/") 'point-redo))

;; --------------------------------------------------------------------------------------------------------------
;; rainbow-delimiters-mode
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((emacs-lisp-mode
          python-mode
          c++-mode
          c-mode) . rainbow-delimiters-mode))

;; --------------------------------------------------------------------------------------------------------------
;; indent-guide
;; (use-package indent-guide
;;   :config
;;   (indent-guide-global-mode t))

;; --------------------------------------------------------------------------------------------------------------
;; spaceline
;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (setq powerline-default-separator nil
;;         spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   (spaceline-spacemacs-theme))

;; (use-package spaceline-all-the-icons 
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

;; --------------------------------------------------------------------------------------------------------------
;; mini-modeline
;; (use-package mini-modeline
;;   :config
;;   (mini-modeline-mode t)
;;   (setq mini-modeline-r-format 
;;         (list
;;          "  "
;;          "%&"
;;          mode-line-buffer-identification
;;          '(vc-mode vc-mode)
;;          "    "
;;          mode-line-modes
;;          "    "
;;          mode-line-misc-info)))

;; --------------------------------------------------------------------------------------------------------------
;; company
(use-package company
  :diminish
  :config
  (global-company-mode t)
  (setq completion-styles '(orderless basic substring partial-completion emacs22)
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-require-match nil
        company-search-regexp-function 'company-search-words-in-any-order-regexp
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
        ;; company-backends '(company-capf))

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
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (company-statistics-mode nil))

;; company-prescient
(use-package company-prescient
  :commands company-prescient-mode
  :hook (company-mode . company-prescient-mode))

;; company-quickhelp
;; (use-package company-quickhelp
;;   :commands company-quickhelp-mode
;;   :hook (company-mode . company-quickhelp-mode)
;;   )

;; --------------------------------------------------------------------------------------------------------------
;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode t)
  (setq yas-snippet-dirs (list (concat emacs-dir "/snippets"))))

;; --------------------------------------------------------------------------------------------------------------
;; blacken
(use-package blacken
  :config
  (setq blacken-line-length 120))

;; --------------------------------------------------------------------------------------------------------------
;; solaire
(use-package solaire-mode
  :config
  (solaire-global-mode t))

;; intellisense
(load "intellisense")

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
;; magit
;; (define-key hs-leader-map (kbd "ms") 'magit-status)

;; --------------------------------------------------------------------------------------------------------------
;; blamer
(use-package blamer
  ;; :quelpa ((blamer :fetcher github :repo "artawower/blamer.el") :upgrade t)
  :quelpa ((blamer :fetcher github :repo "artawower/blamer.el"))
  :ensure t
  ;; :bind (("s-i" . blamer-show-commit-info)
  ;;        ("C-c i" . ("s-i" . blamer-show-posframe-commit-info)))
  :defer 5
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
  ;; (blamer-type 'posframe-popup)
  (blamer-type 'overlay-popup)
  (blamer--overlay-popup-position 'smart)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  ;; :config
  ;; (global-blamer-mode 1))
  )

