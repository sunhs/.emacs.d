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
(defun my-fci-conf ()
  (fci-mode 1)
  (setq fci-rule-column 80)
  (setq fci-rule-color "#40e0d0")
  (setq fci-rule-use-dashes t))
(add-hook 'python-mode-hook 'my-fci-conf)

;; --------------------------------------------------------------------------------------------------------------
;; smooth-scrollint
(require 'smooth-scrolling)

;; --------------------------------------------------------------------------------------------------------------
;; multiple-cursors
(require 'multiple-cursors)

;; key bindings
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-c w") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-c r") 'set-rectangular-region-anchor)

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
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-auto-refresh nil)
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
;; inertial-scroll
(require 'inertial-scroll)
(inertias-global-minor-mode 1)
(setq inertias-initial-velocity-wheel 1)

;; --------------------------------------------------------------------------------------------------------------
;; ivy-mode
(ivy-mode 1)
(global-set-key (kbd "C-c C-x C-f") 'counsel-recentf)

;; --------------------------------------------------------------------------------------------------------------
;; spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; --------------------------------------------------------------------------------------------------------------
;; spaceline-all-the-icons
;; (require 'spaceline-all-the-icons)
;; (spaceline-all-the-icons-theme)

;; --------------------------------------------------------------------------------------------------------------
;; config relating to platforms
(load "local-config-loader")