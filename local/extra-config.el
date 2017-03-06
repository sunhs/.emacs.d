(load "utils")

;; --------------------------------------------------------------------------------------------------------------
;; swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)

;; --------------------------------------------------------------------------------------------------------------
;; indent-guide
(require 'indent-guide)
(indent-guide-global-mode)

;; --------------------------------------------------------------------------------------------------------------
;; point-undo
(require 'point-undo)
(global-set-key (kbd "C-c C-x u") 'point-undo)
(global-set-key (kbd "C-c C-x r") 'point-redo)

;; --------------------------------------------------------------------------------------------------------------
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "M-.") 'undo-tree-undo)
(global-set-key (kbd "M-,") 'undo-tree-redo)

;; --------------------------------------------------------------------------------------------------------------
;; recentf
(require 'recentf)
(recentf-mode 1)
(add-hook 'kill-emacs-hook 'recentf-save-list)

;; --------------------------------------------------------------------------------------------------------------
(require 'fill-column-indicator)
(fci-mode 1)
(setq fci-rule-column 80)
(setq fci-rule-color "#ffffff")
(setq fci-rule-use-dashes t)

;; --------------------------------------------------------------------------------------------------------------
;; smooth-scrollint
(require 'smooth-scrolling)

;; --------------------------------------------------------------------------------------------------------------
;; ;; multiple-cursors
;; (require 'multiple-cursors)

;; ;; key bindings
;; (global-set-key (kbd "C-c l") 'mc/edit-lines)
;; (global-set-key (kbd "C-c w") 'mc/mark-next-word-like-this)
;; (global-set-key (kbd "C-c r") 'set-rectangular-region-anchor)

;; --------------------------------------------------------------------------------------------------------------
;; neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; --------------------------------------------------------------------------------------------------------------
;; web-mode
(add-to-list 'auto-mode-alist '("\\.\\(php\\|ejs\\|html\\|js\\)$" . web-mode))
(add-hook 'web-mode-hook '(lambda () (setq web-mode-code-indent-offset 2)))

;; --------------------------------------------------------------------------------------------------------------
;; sr-speedbar
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "C-c s") 'sr-speedbar-open)

;; --------------------------------------------------------------------------------------------------------------
;; inertial-scroll
(require 'inertial-scroll)
(inertias-global-minor-mode 1)
(setq inertias-initial-velocity-wheel 1)

;; --------------------------------------------------------------------------------------------------------------
;; ivy-mode
(ivy-mode 1)
(global-set-key (kbd "C-c C-x C-f") 'ivy-recentf)

;; --------------------------------------------------------------------------------------------------------------
;; config relating to platforms
(load "local-config-loader")
