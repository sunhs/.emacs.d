(load "utils")

;; specific settings on gnu/linux
(hs-call-or-add-to-frame-hook
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
	(setq interprogram-paste-function 'xsel-paste-function)))

;; to prompt root privileges for non-writable files
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defconst dropbox-dir (substitute-in-file-name "$HOME/Dropbox"))

;; --------------------------------------------------------------------------------------------------------------
;; markdown preview mode
(require 'markdown-preview-mode)
(setq markdown-preview-style "file:///home/edward/.emacs.d/nonelpa/GitHub.css")

;; --------------------------------------------------------------------------------------------------------------
;; org-mode
(require 'org)
;; (add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
		  (lambda () (setq truncate-lines nil)))
(setq org-log-done 'note)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c i") 'org-iswitchb)
(define-key org-mode-map (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; export options
(setq org-export-with-toc nil)

;; --------------------------------------------------------------------------------------------------------------
;; flycheck
(require 'flycheck)
(defun my-py-flycheck-config ()
  (flycheck-mode)
  (setq flycheck-python-pylint-executable
		(substitute-in-file-name "$HOME/anaconda3/bin/pylint"))
  (flycheck-select-checker 'python-pylint))
(add-hook 'python-mode-hook 'my-py-flycheck-config)
(setq flycheck-check-syntax-automatically '(save))

;; --------------------------------------------------------------------------------------------------------------
;; matlab
(defun disable-auto-fill-matlab ()
  (auto-fill-mode -1))
(add-hook 'matlab-mode-hook 'disable-auto-fill-matlab)

;; --------------------------------------------------------------------------------------------------------------
;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'popup)

;; for C
(defun my-ac-cc-mode-setup ()
  (require 'auto-complete-clang)
  ;; run shell command:
  ;; echo "" | g++ -v -x c++ -E -
  (setq ac-clang-flags
		(mapcar (lambda (item)(concat "-I" item))
				(split-string
				 "/usr/include/c++/4.8
 /usr/include/x86_64-linux-gnu/c++/4.8
 /usr/include/c++/4.8/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include"
				 )))
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(defun my-ac-config()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-to-list 'ac-modes 'matlab-mode)
  (global-auto-complete-mode t))
(my-ac-config)

;; display ac menu immediately
(setq ac-auto-show-menu 0)

;; key bindings for menu exploring
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" nil)
(define-key ac-menu-map "\C-p" nil)
(define-key ac-menu-map "\M-n" 'ac-next)
(define-key ac-menu-map "\M-p" 'ac-previous)

;; menu height
(setq ac-menu-height 12)

;; help
(setq ac-use-quick-help t)
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)
(setq ac-quick-help-delay 1)))

;; --------------------------------------------------------------------------------------------------------------
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
(add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
(load-theme 'solarized t)

;; --------------------------------------------------------------------------------------------------------------
;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-command
	  `(,(substitute-in-file-name "$HOME/anaconda3/bin/python") ,(substitute-in-file-name "$HOME/anaconda3/lib/python3.6/site-packages/jediepcserver.py")))
(setq jedi:complete-on-dot t)
(eval-after-load "jedi"
  '(progn
     (define-key jedi-mode-map (kbd "C-c C-x TAB") 'jedi:complete)))

;; --------------------------------------------------------------------------------------------------------------
;; autopep8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))
