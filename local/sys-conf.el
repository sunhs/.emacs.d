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
