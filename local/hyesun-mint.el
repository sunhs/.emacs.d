;; to prompt root privileges for non-writable files
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defconst droptox-dir "~/Dropbox")

;; (set-frame-position (selected-frame) 80 40)
;; (set-frame-width (selected-frame) 140)
;; (set-frame-height (selected-frame) 40)

;; highlight current line
(global-hl-line-mode 1)

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
  (flycheck-select-checker 'python-pylint))
(add-hook 'python-mode-hook 'my-py-flycheck-config)
(setq flycheck-check-syntax-automatically '(save))

;; --------------------------------------------------------------------------------------------------------------
;; auto-complete
(require 'auto-complete-config)
(require 'popup)
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

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(defun my-ac-config()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
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

;; no help
(setq ac-use-quick-help nil)

(call-or-add-to-frame-hook
 (lambda ()
   (when (display-graphic-p)
     ;; use help
     (setq ac-use-quick-help t)
     ;; use pos-tip
     (require 'pos-tip)
     (setq ac-quick-help-prefer-pos-tip t)
     ;; display help after 1s
     (setq ac-quick-help-delay 1))
   (unless (display-graphic-p)
	 (setq ac-use-quick-help nil))))

;; --------------------------------------------------------------------------------------------------------------
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
(add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
(if (display-graphic-p)
	(load-theme 'tsdh-dark t)
  (load-theme 'zsh t))
