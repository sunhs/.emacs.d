;; elpa packages
(require 'package)
(dolist (source '(("melpa-stable" . "https://stable.melpa.org/packages/")
				  ("melpa" . "https://melpa.org/packages/")
				  ("marmalade" . "https://marmalade-repo.org/packages/")
				  ("gnu" . "https://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives source t))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
		("melpa" . 15)
        ("marmalade" . 10)
        ("gnu" . 5)))
(package-initialize)
(setq package-enable-at-startup nil)

;; define load paths
(setq default-directory "~/")
(defconst elpa-dir "~/.emacs.d/elpa")
(defconst nonelpa-dir "~/.emacs.d/nonelpa")
(defconst local-dir "~/.emacs.d/local")
(add-to-list 'load-path elpa-dir)
(add-to-list 'load-path nonelpa-dir)
(add-to-list 'load-path local-dir)

;; key-bindings independent of packages
(load "kbd")

;; utils
(load "utils")

;; benchmark
(benchmark-init/activate)

;; alias UTF-8 as utf-8
(define-coding-system-alias 'UTF-8 'utf-8)

;; make scoll smooth
(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 10000)

;; making buffers with the same name distinguishable
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; disable start-up message
(setq inhibit-startup-message t)

;; disable tool-bar
(tool-bar-mode -1)

;; disable menu-bar
(menu-bar-mode -1)

;; disable side-bar
(scroll-bar-mode -1)

;; do not show mode line
(setq-default mode-line-format nil)

;; display line numbers
;; (require 'linum)
;; (global-linum-mode t) ;; always show line numbers
;; (setq linum-format "%4d| ") ;; linum format

;; tab indent
(setq-default c-basic-offset 4
              tab-width 4)
(defun my-indent-setup()
  (c-set-offset 'arglist-intro '+))
(add-hook 'c++-mode-hook 'my-indent-setup)
(add-hook 'c-mode-hook 'my-indent-setup)
(setq python-indent-offset 4)

;; use y/n to replace yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; no back-up or auto-save files
(setq make-backup-files nil) ;; no backup files
(setq auto-save-mode nil) ;; no auto save
(setq-default make-backup-files nil) ;; no temp file
(setq backup-inhibited t) ;; no backup
(setq auto-save-default nil) ;; no #file#

;; show paren match
(show-paren-mode 1)

;; split windows vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; config relating to platforms
(load "local-config-loader")

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(websocket web-mode undo-tree sr-speedbar smooth-scrolling pos-tip point-undo neotree multiple-cursors matlab-mode markdown-mode lua-mode jedi-direx go-mode geiser flycheck fill-column-indicator csharp-mode counsel color-theme-sanityinc-solarized benchmark-init auto-complete-clang async ac-slime ac-math))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
