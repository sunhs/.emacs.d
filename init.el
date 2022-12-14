;; -*- lexical-binding: t -*-
;; -*- mode: emacs-lisp -*-
;; This is the entry point of init code.
;; The code included here is about built-in config.
;; `utils` and `kbd` are both about built-in settings.
;; `extra-config` is about system-independent package-specific config.
;; `local-config-[OS]` is about system-specific config.

(defconst emacs-dir (substitute-in-file-name "$HOME/.config/emacs"))

(defconst lisp-dir (concat emacs-dir "/lisp"))
(defconst nonelpa-dir (concat emacs-dir "/nonelpa"))
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path nonelpa-dir)

(require 'package)

(let ((emacs-china-dir
       (concat emacs-dir "/emacs-china")))
  (if (file-exists-p emacs-china-dir)
      (setq package-archives `(("melpa" . ,(concat emacs-china-dir "/melpa"))
                               ("melpa-stable" . ,(concat emacs-china-dir "/melpa-stable"))
                               ("marmalade" . ,(concat emacs-china-dir "/marmalade"))
                               ("gnu" . ,(concat emacs-china-dir "/gnu"))))
    
    (setq package-archives
          '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
            ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
            ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))))


(setq package-archive-priorities
      '(("melpa" . 20)
        ("melpa-stable" . 15)
        ("gnu" . 10)
        ("marmalade" . 5)))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'kbd)
(require 'builtin)
;; (require 'evilize)
(setq hs/use-evil-p nil)
;; (if hs/use-evil-p
;;     (hs/toggle-evil t)
;;   (hs/toggle-evil -1))
(require 'org-config)
(require 'extra-config)
(require 'killboard)
(add-to-list 'load-path (concat lisp-dir "/eshell"))
(require 'hyeshell-config)
(add-to-list 'load-path (concat lisp-dir "/lang"))
(require 'intellisense)
(require 'dap)
(require 'lang-cpp)
(require 'lang-python)
(require 'lang-elisp)
(require 'lang-other)
(add-to-list 'load-path (concat lisp-dir "/platform"))
(cond ((string= system-type "gnu/linux") (require 'platform-config-linux))
      ((string= system-type "darwin") (require 'platform-config-mac)))
(if (f-exists? (concat lisp-dir "/theme.el"))
    (load "theme"))
