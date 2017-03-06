(load "utils")

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
						 (font-spec :family "Microsoft Yahei" :size 14))))))

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
(setq markdown-preview-style "file:///Users/edward/.emacs.d/nonelpa/GitHub.css")

;; --------------------------------------------------------------------------------------------------------------
;; org-mode
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
		  (lambda () (setq truncate-lines nil)))
(setq org-log-done 'note)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(eval-after-load "org"
  '(progn
	 (define-key org-mode-map (kbd "C-c l") 'org-store-link)
	 (define-key org-mode-map (kbd "C-c i") 'org-iswitchb)
	 (define-key org-mode-map (kbd "C-j") 'backward-char)))

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
;; latex
(add-to-list 'auto-mode-alist '("\\.\\(tex\\|latex\\|ltx\\)$" . latex-mode))

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
				 "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.0.0/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include
 /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks
 /Users/edward/unpv13e/lib"
				 )))
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

;; for LaTeX
(defun my-ac-latex-mode-setup ()
  (require 'ac-math)
  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-sources
		(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands) ac-sources))
  (setq ac-math-unicode-in-math-p t))

(defun my-ac-config()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-hook 'latex-mode-hook 'my-ac-latex-mode-setup)
  (global-auto-complete-mode t))
(my-ac-config)
(setq ac-math-unicode-in-math-p t)

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
(load-theme 'zsh t)

;; --------------------------------------------------------------------------------------------------------------
;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(eval-after-load "jedi"
  '(progn
     (define-key jedi-mode-map (kbd "C-c C-x <tab>") 'jedi:complete)))
