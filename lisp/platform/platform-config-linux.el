(require 'utils)

;; X-clipboard
(setq x-select-enable-clipboard t)
(unless window-system
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region
          (point-min)
          (point-max)
          "xsel"
          nil
          0
          nil
          "--clipboard"
          "--input")))
    (defun xsel-paste-function ()
      (let
        (
          (xsel-output
            (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output)))
    (setq
      interprogram-cut-function
      'xsel-cut-function
      interprogram-paste-function 'xsel-paste-function)))

;; to prompt root privileges for non-writable files
;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(let*
  (
    (linux-machine-directory
      (concat lisp-dir "/platform/linux-machines"))
    (linux-machine-config "mlplat-sg-dev1")
    (linux-machine-config-el (concat linux-machine-config ".el")))
  (if
    (and
      (f-directory? linux-machine-directory)
      (f-exists?
        (expand-file-name
          linux-machine-config-el
          linux-machine-directory)))
    (progn
      (add-to-list 'load-path linux-machine-directory)
      (load linux-machine-config))))

(provide 'platform-config-linux)
