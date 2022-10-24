(require 'utils)

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
;; themes
;; (require 'color-theme)
;; (color-theme-initialize)
;; (add-to-list 'custom-theme-load-path (concat nonelpa-dir "/themes"))
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'doom-one t)
;; (load-theme 'doom-vibrant t)
;; (load-theme 'gruvbox-dark-soft t)
;; (load-theme 'gruvbox-dark-medium t)
;; (load-theme 'doom-monokai-spectrum t)
;; (load-theme 'doom-monokai-pro t)
;; (load-theme 'doom-monokai-classic t)
;; (load-theme 'doom-snazzy t)
;; (load-theme 'doom-dark+ t)
;; (load-theme 'doom-zenburn t)
(load-theme 'doom-monokai-ristretto t)

(provide 'platform-config-linux)
