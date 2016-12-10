;; elpa packages
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
				  ("marmalade" . "http://marmalade-repo.org/packages/")
				  ("elpa" . "http://tromey.com/elpa/")
				  ))
  (add-to-list 'package-archives source t))
(setq package-enable-at-startup nil)
(package-initialize)

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

;; package list loading and saving
(load "package-list-setter")
(load "utils")
(defconst package-list-file (concat local-dir "/" package-list-file-name))

(if (gen-non-activated-package-list-from-file package-list-file)
	(progn
	  ;; buffer of non-activated-package-list
	  (setq naplb (buffer-name (generate-new-buffer "naplb")))
	  (print (gen-non-activated-package-list-from-file package-list-file)
			 (get-buffer naplb)))
  nil)

(defun install-non-activated-packages ()
  (interactive)
  (if (gen-non-activated-package-list-from-file package-list-file)
	  (install-packages (gen-non-activated-package-list-from-file package-list-file))
	nil))

(defun save-package-list ()
  (save-package-list-to-file package-list-file))
(add-hook 'kill-emacs-hook 'save-package-list)

(benchmark-init/activate)

(require 'cl) ;; to ensure that lexical-let works
(defun call-or-add-to-frame-hook (fun)
  "`fun: A function receiving an optional parameter `frame.
   The purpose of `fun is to decide whether the frame is graphic and
   thus turn on graphic features.
   But in daemon mode, this is decided after the client frame is made.
   Thus we call `fun immediately in normal mode while in daemon mode
   add it to make frame hook.
   For client frames to work normally, `fun should explicitly
   turn on or off graphic features."
  (if (daemonp)
	  (lexical-let ((fun fun))
		(add-hook 'after-make-frame-functions
				  (lambda (frame)
					(select-frame frame)
					(funcall fun))))
	(funcall fun)))

;; alias UTF-8 as utf-8
(define-coding-system-alias 'UTF-8 'utf-8)

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
(add-hook 'web-mode-hook (lambda () (setq web-mode-code-indent-offset 2)))

;; --------------------------------------------------------------------------------------------------------------
;; inertial-scroll
(require 'inertial-scroll)
(inertias-global-minor-mode 1)
(setq inertias-initial-velocity-wheel 1)

;; --------------------------------------------------------------------------------------------------------------
;; ivy-mode
(ivy-mode 1)
(global-set-key (kbd "C-c C-x C-f") 'ivy-recentf)
