(require 'cl) ;; to ensure that lexical-let works

;; custom key bindings
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-right)
(global-set-key (kbd "C-c C-x m") 'set-mark-command)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c /") 'uncomment-region)
(global-set-key (kbd "C-c C-f") 'load-file)
(global-set-key (kbd "C-c C-x C-f") 'ivy-recentf)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-x s") 'replace-string)
;; (define-key (current-global-map) [remap newline] 'newline-and-indent)

(defun show-file-path ()
  "show the path of file in the current buffer"
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c C-x f") 'show-file-path)

(defun select-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1))
(global-set-key (kbd "C-c C-x l") 'select-line)

(defun delete-line ()
  (interactive)
  (select-line)
  (backward-delete-char-untabify 1))
(global-set-key (kbd "C-c C-x x") 'delete-line)

(defun comment-line ()
  (interactive)
  (select-line)
  (comment-region (mark) (point)))
(global-set-key (kbd "C-c C-x ;") 'comment-line)

(defun uncomment-line ()
  (interactive)
  (select-line)
  (uncomment-region (mark) (point)))
(global-set-key (kbd "C-c C-x /") 'uncomment-line)

(defun call-or-add-to-frame-hook (fun)
  "`fun: A function receiving an optional parameter `frame.
   The purpose of `fun is to decide whether the frame is graphic and
   thus turn on graphic features.
   But in daemon mode, this is decided after the client frame is made.
   Thus we call `fun immediately in normal mode while in daemon mode
   add it to make frame hook. For client frames to work normally, `fun
   should explicitly turn on or off graphic features."
  (if (daemonp)
	  (lexical-let ((fun fun))
		(add-hook 'after-make-frame-functions
				  (lambda (frame)
					(select-frame frame)
					(funcall fun))))
	(funcall fun)))

;; specific settings on os x
(when (eq system-type 'darwin)
  ;; special settings for emacs mac port
  (setq mac-option-modifier (quote (:ordinary meta :function alt :mouse alt)))
  (setq mac-pass-command-to-system nil)
  (setq mac-system-move-file-to-trash-use-finder t)

  (setq ns-pop-up-frames nil) ;; don't open file in a new frame
  (setq mac-command-modifier 'control) ;; map command to control
  ;; display chinese fonts normally in GUI
  (set-default-font "Monaco 14")
  (call-or-add-to-frame-hook
   (lambda ()
	 (when (display-graphic-p)
       (dolist (charset '(kana han symbol cjk-misc bopomofo))
		 (set-fontset-font (frame-parameter nil 'font)
						   charset
						   (font-spec :family "Microsoft Yahei" :size 14)))))))

;; specific settings on gnu/linux
(when (eq system-type 'gnu/linux)
  (call-or-add-to-frame-hook
   (lambda ()
     (when (display-graphic-p)
	   (set-default-font "Mono 11")
       (dolist (charset '(kana han symbol cjk-misc bopomofo))
		 (set-fontset-font (frame-parameter nil 'font)
						   charset
						   (font-spec :family "Droid Sans Fallback" :size 15))))))
  ;; X-clipboard
  (setq x-select-enable-clipboard t)
  (unless window-system
    (when (getenv "DISPLAY")
      (defun xsel-cut-function (text &optional push)
		(with-temp-buffer
		  (insert text)
		  (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
      (defun xsel-paste-function()
		(let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
		  (unless (string= (car kill-ring) xsel-output)
			xsel-output)))
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function))))

;; specific settings on windows
(when (eq system-type 'windows-nt)
  ;; display chinese fonts normally in GUI
  (set-default-font "Verdana 12")
  (call-or-add-to-frame-hook
   (lambda ()
     (when (display-graphic-p)
       (dolist (charset '(kana han symbol cjk-misc bopomofo))
		 (set-fontset-font (frame-parameter nil 'font)
						   charset
						   (font-spec :family "Microsoft Yahei" :size 14)))))))

(setq default-directory "~/")
(defconst elpa-dir "~/.emacs.d/elpa")
(defconst nonelpa-dir "~/.emacs.d/nonelpa")
(defconst local-dir "~/.emacs.d/local")
(defconst package-list-file (concat local-dir "/package-list"))
(add-to-list 'load-path elpa-dir)
(add-to-list 'load-path nonelpa-dir)
(add-to-list 'load-path local-dir)

;; alias UTF-8 as utf-8
(define-coding-system-alias 'UTF-8 'utf-8)

;; making buffers with the same name distinguishable
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; disable start-up message
(setq inhibit-startup-message t)

;; disable tool-bar
(tool-bar-mode -1)

;; display line numbers
(require 'linum)
(global-linum-mode t) ;; always show line numbers
(setq linum-format "%4d| ") ;; linum format

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

;; elpa packages
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
				  ("marmalade" . "http://marmalade-repo.org/packages/")
				  ("elpa" . "http://tromey.com/elpa/")
				  ))
  (add-to-list 'package-archives source t))
(package-initialize)

(load "utils")
(load "local-config-loader")

(setq package-list (read-package-list-from-file package-list-file))
(dolist (p package-list)
  (unless (package-installed-p p)
	(package-install p)))

(defun save-package-list ()
  (save-package-list-to-file package-list-file))
(add-hook 'kill-emacs-hook 'save-package-list)

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

;; --------------------------------------------------------------------------------------------------------------
;; recentf
(require 'recentf)
(recentf-mode 1)
(add-hook 'kill-emacs-hook 'recentf-save-list)

;; --------------------------------------------------------------------------------------------------------------
(require 'fill-column-indicator)
(defun my-fci-setup ()
  (fci-mode 1)
  (setq fci-rule-column 80)
  (setq fci-rule-color "#ffffff")
  (setq fci-rule-use-dashes t))
(add-hook 'c-mode-hook 'my-fci-setup)
(add-hook 'c++-mode-hook 'my-fci-setup)
(add-hook 'python-mode-hook 'my-fci-setup)

;; --------------------------------------------------------------------------------------------------------------
;; smooth-scrollint
(require 'smooth-scrolling)

;; --------------------------------------------------------------------------------------------------------------
;; multiple-cursors
(require 'multiple-cursors)

;; key bindings
(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-c w") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-c r") 'set-rectangular-region-anchor)

;; --------------------------------------------------------------------------------------------------------------
;; neotree
(require 'neotree)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; --------------------------------------------------------------------------------------------------------------
;; web-mode
(require 'web-mode)
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

;; --------------------------------------------------------------------------------------------------------------
;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(eval-after-load "jedi"
  '(progn
     (define-key jedi-mode-map (kbd "C-c C-x <tab>") 'jedi:complete)))
