(require 'utils)

;; specific settings on gnu/linux
(set-default-font "Mono 12")
(hyesun/call-or-add-to-frame-hook
 (lambda ()
   (when (display-graphic-p)
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
    (setq interprogram-cut-function 'xsel-cut-function
          interprogram-paste-function 'xsel-paste-function)))

;; to prompt root privileges for non-writable files
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; --------------------------------------------------------------------------------------------------------------
;; dired-sidebar
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-face '(:family "Mono" :size 12))
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (define-key dired-mode-map (kbd "SPC") spc-leader-map))

;; --------------------------------------------------------------------------------------------------------------
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
;; (add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
;; (load-theme 'solarized t)
;; (require 'spacemacs-dark-theme)
;; (load-theme 'atom-one-dark t)
(require 'abyss-theme)

;; --------------------------------------------------------------------------------------------------------------
;; python path for anaconda mode
(if (file-exists-p "/usr/bin/python3.5")
    (setq python-shell-interpreter "/usr/bin/python3.5"))
