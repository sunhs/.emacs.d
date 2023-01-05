;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-

(use-package smartparens
  :config
  (smartparens-global-mode)
  (require 'smartparens-config)

  (defvar sp-mode-map (make-sparse-keymap))
  (global-set-key (kbd "M-9") sp-mode-map)
  (define-key sp-mode-map (kbd "(") 'sp-beginning-of-sexp)
  (define-key sp-mode-map (kbd ")") 'sp-end-of-sexp)
  (define-key sp-mode-map (kbd "{") 'sp-backward-up-sexp)
  (define-key sp-mode-map (kbd "}") 'sp-up-sexp)
  (define-key sp-mode-map (kbd "[") 'sp-backward-down-sexp)
  (define-key sp-mode-map (kbd "]") 'sp-down-sexp)
  (define-key sp-mode-map (kbd "<") 'sp-previous-sexp)
  (define-key sp-mode-map (kbd ">") 'sp-next-sexp)
  (setq prohibition-sym-list
    '("\\\\(" "\\{" "\\(" "\\\"" "\"" "'" "`" "(" "[" "{"))
  (dolist (sym prohibition-sym-list)
    (sp-pair
      sym
      nil
      :unless '(sp-point-before-word-p sp-point-before-same-p)))

  (defvar hs/enclosed-region-start-sym-list '("'" "\"" "(" "[" "{"))
  (defvar hs--equal-pair-sym-list '("'" "\""))
  (defvar hs/enclosed-region-mode-list '("i" "a"))

  (defun hs--select-enclosed-region (left mode)
    (let*
      (
        (prompt
          (format
            "Start symbol (choices: %s): "
            (string-join hs/enclosed-region-start-sym-list " ")))
        (init-point (point))
        (last-point)
        (start)
        (end))

      (if
        (or
          (not left)
          (not
            (cl-find
              left
              hs/enclosed-region-start-sym-list
              :test #'string=))
          (not mode)
          (not
            (cl-find
              mode
              hs/enclosed-region-mode-list
              :test #'string=)))
        nil

        ;; Already on `left', may need to move forward.
        ;; Otherwise, `sp-beginning-of-sexp' may move back beyond the desired region.
        (if (string= (char-to-string (char-after)) left)
          (let (open)
            (
              cond
              ;; may be " or ', need to determine whether on open or close position
              (
                (cl-find left hs--equal-pair-sym-list :test #'string=)
                ;; try move to beginning of sexp
                ;; 1) if on open position, after moving, char before point will not be the same symbol
                ;; 2) if on close position, after moving, char before point will be the same symbol
                (sp-beginning-of-sexp)
                (if (string= (char-to-string (char-before)) left)
                  nil
                  (setq open t))
                (goto-char init-point))
              ;; otherwise, like ( or [, etc, must be on open position
              (t
                (setq open t)))
            (if open
              (forward-char))))

        (setq last-point (point))

        (sp-beginning-of-sexp)
        (while
          (and
            (not (= (point) last-point)) ;; moved by `sp-beginning-of-sexp'
            (not (string= (char-to-string (char-before)) left)))
          (sp-backward-up-sexp)
          (setq last-point (point))
          (sp-beginning-of-sexp))

        (if (not (string= (char-to-string (char-before)) left))
          (goto-char init-point) ;; still not found, go back like nothing happened

          (setq start (point))
          (sp-end-of-sexp)
          (setq end (point))

          (when (string= mode "a")
            (cl-decf start)
            (cl-incf end))

          (push-mark start)
          (activate-mark)
          (goto-char end)))))

  (dolist (sym hs/enclosed-region-start-sym-list)
    (dolist (mode hs/enclosed-region-mode-list)
      (define-key sp-mode-map (kbd (concat mode sym))
        #'
        (lambda ()
          (interactive)
          (hs--select-enclosed-region sym mode)))))

  ;; (define-key sp-mode-map (kbd "i") 'hs/select-in-enclosed-region)
  ;; (define-key sp-mode-map (kbd "a")
  ;;   'hs/select-around-enclosed-region)
  (defun hs-cmd/jump-enclosed-sym ()
    (interactive)
    (let ((init-point (point)))
      (sp-beginning-of-sexp)
      (if (= (point) init-point)
        (sp-end-of-sexp))))
  (global-set-key (kbd "M-0") #'hs-cmd/jump-enclosed-sym))


(use-package projectile
  :config
  (projectile-mode t)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-sort-order 'recently-active)
  (define-key hs-leader-map "p" 'projectile-command-map))

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ivy / swiper / counsel >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

;; (use-package counsel-projectile
;;   :config
;;   (counsel-projectile-mode t)
;;   (define-key hs-leader-map "p" 'projectile-command-map)
;;   (define-key projectile-command-map "f" 'projectile-find-file)
;;   (define-key projectile-command-map "d" 'projectile-find-dir)
;;   (define-key projectile-command-map "p" 'projectile-switch-project)
;;   (define-key projectile-command-map "b" 'projectile-switch-to-buffer)
;;   (define-key projectile-command-map "g" 'projectile-ripgrep))

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< vertico / consult / orderless >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; -------------------------------------------------- vertico ---------------------------------------------------
(use-package vertico
  :init (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 18)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :config
  ;; define it here rather than evil, to better switch between ivy/counsel/swiper and vertico/consult
  (define-key hs-leader-map "ff" 'find-file)
  (define-key hs-leader-map (kbd "SPC") 'execute-extended-command)

  :bind
  (:map
    vertico-map
    ("C-v" . (lambda () (interactive) (vertico-next 10)))
    ("M-v" . (lambda () (interactive) (vertico-previous 10)))))

(use-package vertico-prescient
  :commands vertico-prescient-mode
  :hook (vertico-mode . vertico-prescient-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

;; -------------------------------------------------- consult ---------------------------------------------------
(use-package consult
  :init (recentf-mode)

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  (setq recentf-max-saved-items 500)
  (setq consult-project-function #'projectile-project-root)
  ;; set this to true so `projectile-switch-project-action' will work
  (setq consult-projectile-use-projectile-switch-project t)
  (setq projectile-switch-project-action
    #'
    (lambda ()
      (consult-projectile
        '
        (consult-projectile--source-projectile-buffer
          consult-projectile--source-projectile-file))))
  (global-set-key (kbd "C-s") 'consult-line)
  (define-key minibuffer-mode-map (kbd "C-s") 'consult-history)
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (global-set-key (kbd "M-g g") 'consult-goto-line)
  (define-key hs-leader-map (kbd "bb") 'consult-buffer)
  (define-key projectile-command-map "g" 'consult-ripgrep))

(use-package consult-projectile
  :config
  (global-set-key (kbd "M-P") 'consult-projectile)
  (define-key projectile-command-map "f"
    'consult-projectile-find-file)
  (define-key projectile-command-map "d" 'consult-projectile-find-dir)
  (define-key projectile-command-map "p"
    'consult-projectile-switch-project)
  (define-key projectile-command-map "b"
    'consult-projectile-switch-to-buffer))

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
    :map
    vertico-map
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)))

;; ------------------------------------------------- orderless --------------------------------------------------
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
    completion-styles
    '(orderless basic) ;; overridden in company
    ;; completion-category-defaults nil
    completion-category-overrides
    '((file (styles partial-completion)))))

;; ------------------------------------------------- marginalia -------------------------------------------------
(use-package marginalia
  :config (marginalia-mode))

;; --------------------------------------------------- embark ---------------------------------------------------
(use-package embark
  :bind
  (("M-o" . embark-act) ;; pick some comfortable binding
    ;; ("M-d" . embark-dwim)        ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
    '
    ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      nil
      (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< treemacs >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package treemacs
  :init (define-key hs-leader-map "wt" 'treemacs-select-window)
  :config
  (setq treemacs-follow-after-init t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (define-key hs-leader-map (kbd "tt")
    'treemacs-add-and-display-current-project-exclusively)
  (define-key treemacs-mode-map (kbd "M-l") nil))
;; (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

(if hs/use-evil-p
  (use-package treemacs-evil
    :after
    treemacs
    evil
    :config
    (add-hook 'evil-treemacs-state-entry-hook
      #'(lambda () (setq evil-treemacs-state-cursor '(box))))))

(use-package treemacs-projectile
  :after
  treemacs
  projectile)


;; (use-package dired-sidebar
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :config
;;   (setq dired-sidebar-theme 'nerd)
;;   (setq dired-sidebar-face '(:family "Mono" :size 12))
;;   (setq dired-sidebar-use-term-integration t)
;;   (setq dired-sidebar-use-custom-font t)
;;   (define-key dired-mode-map (kbd "SPC") hs-leader-map))

;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode)
;;   (global-set-key (kbd "M-u") 'undo-tree-undo)
;;   (global-set-key (kbd "M-r") 'undo-tree-redo)
;;   (hs/define-key-when-set hs/use-evil-p evil-normal-state-map "r" 'undo-tree-redo))

(use-package undo-fu
  :config
  ;; (global-undo-tree-mode -1)
  (global-set-key (kbd "M-u") 'undo-fu-only-undo)
  (global-set-key (kbd "M-r") 'undo-fu-only-redo)
  (hs/define-key-when-set
    hs/use-evil-p
    evil-normal-state-map
    "u"
    'undo-fu-only-undo)
  (hs/define-key-when-set
    hs/use-evil-p
    evil-normal-state-map
    "r"
    'undo-fu-only-redo))

;; ------------------------------------------------- point-undo -------------------------------------------------
(require 'point-undo)
(global-set-key (kbd "M-;") 'point-undo)
(global-set-key (kbd "M-/") 'point-redo)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook
  ((emacs-lisp-mode python-mode c++-mode c-mode)
    .
    rainbow-delimiters-mode))

;; (use-package indent-guide
;;   :config
;;   (indent-guide-global-mode t))

;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (setq powerline-default-separator nil
;;         spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   (spaceline-spacemacs-theme))

;; (use-package spaceline-all-the-icons 
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

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

(use-package magit
  :config
  (setq magit-log-margin
    '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

  :bind
  (:map
    hs-leader-map
    ("mg" . magit)
    ("mf" . magit-log-buffer-file)
    ("mba" . magit-blame-addition)
    ("mbq" . magit-blame-quit)))

(use-package blamer
  :defer 5
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
  ;; (blamer-type 'posframe-popup)
  (blamer-type 'overlay-popup)
  (blamer--overlay-popup-position 'smart)
  :custom-face
  (blamer-face
    (
      (t
        :foreground "#7a88cf"
        :background nil
        :height 140
        :italic t)))
  ;; :config
  ;; (global-blamer-mode 1))
  )


;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< company >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package company-statistics
  :config (company-statistics-mode))

;; -------------------------------------------------- company ---------------------------------------------------
(use-package company
  :diminish
  :config
  (global-company-mode t)
  (setq
    completion-styles
    '(orderless basic substring partial-completion emacs22)
    company-idle-delay
    0
    company-minimum-prefix-length
    2
    company-require-match
    nil
    company-search-regexp-function
    'company-search-words-in-any-order-regexp
    company-selection-wrap-around
    t
    company-tooltip-align-annotations
    t
    company-dabbrev-downcase
    nil)
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
  (define-key company-active-map (kbd "C-s")
    'company-filter-candidates)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (company-statistics-mode nil))

(use-package company-prescient
  :commands company-prescient-mode
  :hook (company-mode . company-prescient-mode))

;; (use-package company-quickhelp
;;   :commands company-quickhelp-mode
;;   :hook (company-mode . company-quickhelp-mode)
;;   )


(use-package yasnippet
  :config
  (yas-global-mode t)
  (setq yas-snippet-dirs (list (concat emacs-dir "/snippets"))))

(use-package blacken
  :config (setq blacken-line-length 120))

;; https://ianyepan.github.io/posts/emacs-git-gutter/
;; (use-package git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :config (setq git-gutter:update-interval 0.02))

;; (use-package git-gutter-fringe
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-package solaire-mode
  :config (solaire-global-mode t))

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< themes >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq
    doom-themes-enable-bold
    t ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; from https://github.com/doomemacs/themes/issues/248
  (set-face-attribute
    'font-lock-comment-face
    nil
    :foreground "#5B6268"
    :slant 'italic)
  (set-face-attribute
    'font-lock-function-name-face
    nil
    :foreground "#c678dd"
    :slant 'italic)
  (set-face-attribute
    'font-lock-variable-name-face
    nil
    :foreground "#dcaeea"
    :slant 'italic))

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq
    modus-themes-italic-constructs
    t
    modus-themes-bold-constructs nil
    modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))


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

(provide 'extra-config)
