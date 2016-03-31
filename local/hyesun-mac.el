;; to prompt root privileges for non-writable files
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defconst droptox-dir "~/Dropbox")

(global-hl-line-mode 1)

;; --------------------------------------------------------------------------------------------------------------
;; markdown preview mode
(require 'markdown-preview-mode)
(setq markdown-preview-style "file:///Users/edward/.emacs.d/nonelpa/GitHub.css")

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

;; org-capture
(defconst capture-todo-file (concat droptox-dir "/org/TODO.org"))
(defconst capture-memo-file (concat droptox-dir "/org/MEMO.org"))
(defconst capture-note-file (concat droptox-dir "/org/READING_NOTE.org"))
;; (defconst capture-birthday-file (concat droptox-dir "/org/BIRTHDAY.org"))
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline capture-todo-file "TASK") "** TODO %?\n   %i\n")
        ("m" "MEMO" entry (file+headline capture-memo-file "MEMO") "** %?\n   %i\n")
        ("n" "NOTE" entry (file+headline capture-note-file "NOTE") "** %?\n   %i\n   %a")
        ;; ("b" "BIRTHDAY" entry (file+headline capture-birthday-file "BIRTHDAY") "")
		))

;; agenda
(setq org-agenda-files (quote ("~/Dropbox/org/TODO.org" "~/Dropbox/org/MEMO.org" "~/Dropbox/org/READING_NOTE.org")))
(setq org-agenda-include-diary t)

;; iCalendar
(setq org-icalendar-include-todo t)
(setq org-icalendar-combined-agenda-file (concat droptox-dir "/org/ICSFILE.ics"))

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
			   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.0.0/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include
 /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks
 /Users/edward/unpv13e/lib"
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
   ;; use help
   (setq ac-use-quick-help t)
   ;; use pos-tip
   (require 'pos-tip)
   (setq ac-quick-help-prefer-pos-tip t)
   ;; display help after 1s
   (setq ac-quick-help-delay 1)))

;; --------------------------------------------------------------------------------------------------------------
(require 'geiser)
(setq geiser-default-implementation 'racket)
(setq geiser-implementations-alist '(((regexp "\\.scm$") racket) ((regexp "\\.ss$") racket) ((regexp "\\.rkt") racket)))
(setq geiser-racket-binary "/Applications/racket/bin/racket")
(setq geiser-mode-start-repl-p t)

;; --------------------------------------------------------------------------------------------------------------
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
(add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
(load-theme 'tsdh-dark t)
